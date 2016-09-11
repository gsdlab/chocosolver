package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import org.clafer.collection.DisjointSets;
import org.clafer.collection.Either;
import org.clafer.collection.Triple;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrMul;
import static org.clafer.ir.Irs.add;
import static org.clafer.ir.Irs.equal;
import static org.clafer.ir.Irs.greaterThanEqual;
import static org.clafer.ir.Irs.lessThanEqual;
import static org.clafer.ir.Irs.mul;
import org.clafer.math.LinearEquation;
import org.clafer.math.LinearEquation.Op;
import org.clafer.math.LinearFunction;
import org.clafer.math.LinearFunctionBuilder;
import org.clafer.math.LinearSystem;
import org.clafer.math.Rational;
import org.clafer.math.Variable;

/**
 *
 * @author jimmy
 */
public class LinearEquationOptimizer {

    private static boolean lossy = false;

    private LinearEquationOptimizer() {
    }

    private LinearFunction linearFunction(IrIntExpr expr, Map<IrIntVar, Variable> map) {
        LinearFunctionBuilder builder = new LinearFunctionBuilder();
        return linearFunction(expr, builder, map) ? builder.toFunction() : null;
    }

    private boolean linearFunction(IrIntExpr expr, LinearFunctionBuilder builder, Map<IrIntVar, Variable> map) {
        if (expr instanceof IrIntVar) {
            Domain domain = expr.getDomain();
            if (domain.isConstant()) {
                builder.plusConstant(domain.getLowBound());
            } else {
                IrIntVar var = (IrIntVar) expr;
                Variable variable = map.get(var);
                if (variable == null) {
                    variable = new Variable(var.getName(), var.getLowBound(), var.getHighBound());
                    map.put(var, variable);
                }
                builder.plusTerm(variable);
            }
            return true;
        } else if (expr instanceof IrAdd) {
            IrAdd add = (IrAdd) expr;
            for (IrIntExpr addend : add.getAddends()) {
                if (!linearFunction(addend, builder, map)) {
                    return false;
                }
            }
            builder.plusConstant(add.getOffset());
            return true;
        } else if (expr instanceof IrMul) {
            IrMul mul = (IrMul) expr;
            int coefficient;
            IrIntExpr variable;
            if (mul.getMultiplicand().isConstant()) {
                coefficient = mul.getMultiplicand().getLowBound();
                variable = mul.getMultiplier();
            } else if (mul.getMultiplier().isConstant()) {
                coefficient = mul.getMultiplier().getLowBound();
                variable = mul.getMultiplicand();
            } else {
                return false;
            }
            LinearFunctionBuilder mulBuilder = new LinearFunctionBuilder();
            if (linearFunction(variable, mulBuilder, map)) {
                builder.plusFunction(mulBuilder.toFunction().mul(coefficient));
                return true;
            }
        }
        return false;
    }

    private Triple<LinearEquation, Domain, Domain> linearEquation(IrIntExpr expr, Map<IrIntVar, Variable> map) {
        if (expr instanceof IrCompare) {
            IrCompare compare = (IrCompare) expr;

            if (!NotEqual.equals(compare.getOp())) {
                LinearFunction left = linearFunction(compare.getLeft(), map);
                if (left != null) {
                    LinearFunction right = linearFunction(compare.getRight(), map);
                    if (right != null) {
                        switch (compare.getOp()) {
                            case Equal:
                                return new Triple<>(LinearEquation.equal(left, right),
                                        compare.getLeft().getDomain(), compare.getRight().getDomain());
                            case LessThan:
                                return new Triple<>(LinearEquation.lessThan(left, right),
                                        compare.getLeft().getDomain(), compare.getRight().getDomain());
                            case LessThanEqual:
                                return new Triple<>(LinearEquation.lessThanEqual(left, right),
                                        compare.getLeft().getDomain(), compare.getRight().getDomain());
                        }
                    }
                }
            }
        }
        return null;
    }

    private Optional<LinearEquation> round(LinearEquation equation) {
        LinearFunction left = equation.getLeft();
        Rational[] cs = left.getCoefficients();
        Op op = equation.getOp();
        Rational right = equation.getRight();
        Rational min = right;
        Rational max = right;
        long lcm = cs[0].getDenominator();
        if (Op.Equal.equals(op)) {
            lcm = Util.lcm(lcm, right.getDenominator());
        }
        min = min.min(cs[0]);
        max = max.max(cs[0]);
        for (int i = 1; i < cs.length; i++) {
            Rational c = cs[i];
            min = min.min(c);
            max = max.max(c);
            lcm = Util.lcm(lcm, c.getDenominator());
        }
        Rational multiplier;
        if (Op.LessThanEqual.equals(op)) {
            long gcd = cs[0].mul(lcm).getNumerator();
            for (int i = 1; i < cs.length; i++) {
                Rational c = cs[i];
                gcd = Util.gcd(gcd, c.mul(lcm).getNumerator());
            }
            gcd = Math.abs(gcd);
            multiplier = new Rational(lcm, gcd);
        } else {
            multiplier = new Rational(lcm);
        }
        if (max.mul(multiplier).ceil() < 50000 && min.mul(multiplier).floor() > -50000) {
            return Optional.of(
                    new LinearEquation(equation.getLeft().mul(multiplier),
                            equation.getOp(),
                            Op.Equal.equals(op)
                                    ? equation.getRight().mul(multiplier)
                                    : new Rational(equation.getRight().mul(multiplier).floor()),
                            false)
            );
        }
        lossy = true;
        return Optional.empty();
    }

    private static IrIntExpr scale(int c, IrIntExpr var) {
        int a = c * var.getLowBound();
        int b = c * var.getHighBound();
        return mul(c, var, Domains.boundDomain(Math.min(a, b), Math.max(a, b)));
    }

    private Optional<IrBoolExpr> boolExpr(LinearEquation equation, Map<Variable, IrIntVar> map, int low, int high) {
        Optional<LinearEquation> roundOpt = round(equation);
        if (!roundOpt.isPresent()) {
            return Optional.empty();
        }
        LinearEquation round = roundOpt.get();
        LinearFunction left = round.getLeft();
        Rational[] cs = left.getCoefficients();
        Variable[] vs = left.getVariables();
        Rational right = round.getRight();

        if (right.isZero() && cs.length == 2) {
            int c0 = (int) cs[0].getNumerator();
            IrIntVar v0 = map.get(vs[0]);
            int c1 = (int) cs[1].getNumerator();
            IrIntVar v1 = map.get(vs[1]);
            switch (round.getOp()) {
                case Equal:
                    if (c0 < 0) {
                        return Optional.of(equal(scale(-c0, v0), scale(c1, v1)));
                    }
                    return Optional.of(equal(scale(c0, v0), scale(-c1, v1)));
                case LessThanEqual:
                    if (c0 < 0) {
                        return Optional.of(greaterThanEqual(scale(-c0, v0), scale(c1, v1)));
                    }
                    return Optional.of(lessThanEqual(scale(c0, v0), scale(-c1, v1)));
                default:
                    throw new IllegalStateException();
            }
        }

        IrIntExpr[] addends = new IrIntExpr[cs.length];
        boolean neg = Stream.of(cs).filter(Rational::isNegative).count() * 2 > addends.length;
        for (int j = 0; j < addends.length; j++) {
            assert cs[j].isWhole();
            long coefficient = cs[j].getNumerator();
            int coefficientI = (int) (neg ? -coefficient : coefficient);
            IrIntVar var = map.get(vs[j]);

            addends[j] = scale(coefficientI, var);
        }
        assert right.isWhole();
        switch (round.getOp()) {
            case Equal:
                return Optional.of(equal(add(addends), (int) (neg ? -right.getNumerator() : right.getNumerator())));
            case LessThanEqual:
                if (neg) {
                    return Optional.of(greaterThanEqual(add(addends), (int) -right.getNumerator()));
                } else {
                    return Optional.of(lessThanEqual(add(addends), (int) right.getNumerator()));
                }
            default:
                throw new IllegalStateException();
        }
    }

    private IrModule optimizeImpl(IrModule module) {
        List<IrBoolExpr> constraints = new ArrayList<>();
        Set<LinearEquation> equations = new HashSet<>();
        Map<IrIntVar, Variable> map = new HashMap<>();
        int low = Integer.MAX_VALUE;
        int high = Integer.MIN_VALUE;
        Map<LinearEquation, IrBoolExpr> trace = new HashMap<>();
        for (IrBoolExpr constraint : module.getConstraints()) {
            Triple<LinearEquation, Domain, Domain> pair = linearEquation(constraint, map);
            if (pair != null) {
                LinearEquation equation = pair.getFst();
                Domain d1 = pair.getSnd();
                Domain d2 = pair.getThd();
                equations.add(equation);
                low = Math.min(Math.min(low, d1.getLowBound()), d2.getLowBound());
                high = Math.max(Math.max(high, d1.getHighBound()), d2.getHighBound());
                trace.put(equation, constraint);
            } else {
                constraints.add(constraint);
            }
        }
        if (equations.size() > 0) {
            Map<Variable, IrIntVar> inverse = Util.inverse(map);

            DisjointSets<Either<LinearEquation, Variable>> ds = new DisjointSets<>();
            for (LinearEquation equation : equations) {
                Either<LinearEquation, Variable> left = Either.left(equation);
                for (Variable variable : equation.getVariables()) {
                    Either<LinearEquation, Variable> right = Either.right(variable);
                    ds.union(left, right);
                }
            }
            for (Set<Either<LinearEquation, Variable>> component : ds.connectedComponents()) {
                Set<LinearEquation> componentEquations = Either.filterLeft(component);
                LinearSystem system = new LinearSystem(componentEquations);
                for (LinearEquation equation : system
                        .equalityElimination()
                        .fourierMotzkinElimination()
                        .strengthenInequalities()
                        .gaussJordanElimination()
                        // TODO only add back necessary equations
                        .addEquations(componentEquations)
                        .dominantElimination()
                        .getEquations()) {
                    boolExpr(equation, inverse, low, high).ifPresent(constraints::add);
                }

                if (lossy) {
                    for (LinearEquation componentEquation : componentEquations) {
                        constraints.add(trace.get(componentEquation));
                    }
                }
            }
            return new IrModule().addConstraints(constraints);
        }
        return module;
    }

    public static IrModule optimize(IrModule module) {
        return new LinearEquationOptimizer().optimizeImpl(module);
    }
}
