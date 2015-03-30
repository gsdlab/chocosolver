package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrCompare;
import static org.clafer.ir.IrCompare.Op.NotEqual;
import org.clafer.ir.IrConstant;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrMul;
import static org.clafer.ir.Irs.*;
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

    private LinearEquationOptimizer() {
    }

    private static LinearFunction linearFunction(IrIntExpr expr, Map<IrIntVar, Variable> map) {
        LinearFunctionBuilder builder = new LinearFunctionBuilder();
        return linearFunction(expr, builder, map) ? builder.toFunction() : null;
    }

    private static boolean linearFunction(IrIntExpr expr, LinearFunctionBuilder builder, Map<IrIntVar, Variable> map) {
        if (expr instanceof IrIntVar) {
            Domain domain = expr.getDomain();
            if (domain.size() == 1) {
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
            if (mul.getMultiplicand() instanceof IrConstant) {
                coefficient = mul.getMultiplicand().getLowBound();
                variable = mul.getMultiplier();
            } else if (mul.getMultiplier() instanceof IrConstant) {
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

    private static LinearEquation linearEquation(IrIntExpr expr, Map<IrIntVar, Variable> map) {
        if (expr instanceof IrCompare) {
            IrCompare compare = (IrCompare) expr;

            if (!NotEqual.equals(compare.getOp())) {
                LinearFunction left = linearFunction(compare.getLeft(), map);
                if (left != null) {
                    LinearFunction right = linearFunction(compare.getRight(), map);
                    if (right != null) {
                        switch (compare.getOp()) {
                            case Equal:
                                return LinearEquation.equal(left, right);
                            case LessThan:
                                return LinearEquation.lessThan(left, right);
                            case LessThanEqual:
                                return LinearEquation.lessThanEqual(left, right);
                        }
                    }
                }
            }
        }
        return null;
    }

    private static LinearEquation[] round(LinearEquation equation) {
        LinearFunction left = equation.getLeft();
        Rational[] cs = left.getCoefficients();
        Variable[] vs = left.getVariables();
        Rational right = equation.getRight();
        Rational min = right;
        Rational max = right;
        long lcm = right.getDenominator();
        for (Rational c : cs) {
            if (min.compareTo(c) > 0) {
                min = c;
            }
            if (max.compareTo(c) < 0) {
                max = c;
            }
            lcm = Util.lcm(lcm, c.getDenominator());
        }
        if (max.ceil() * lcm < 50000 && min.floor() * lcm > -50000) {
            return new LinearEquation[]{
                new LinearEquation(equation.getLeft().mul(lcm), equation.getOp(), equation.getRight().mul(lcm), false)
            };
        }
        long multiplier = Math.min(Math.abs(50000 / max.ceil()), Math.abs(50000 / min.floor()));
        if (multiplier == 0) {
            multiplier = 1;
        }
        long[] lIcs = new long[cs.length];
        long[] gIcs = new long[cs.length];
        Rational lR = right.mul(multiplier);
        Rational gR = lR;
        for (int i = 0; i < lIcs.length; i++) {
            Rational c = cs[i].mul(multiplier);
            assert !c.isZero();
            if (c.isPositive()) {
                lIcs[i] = c.floor();
                gIcs[i] = -c.ceil();
                lR = lR.add(vs[i].getHighBound());
                gR = gR.sub(vs[i].getHighBound());
            } else {
                lIcs[i] = c.ceil();
                gIcs[i] = -c.floor();
                lR = lR.sub(vs[i].getLowBound());
                gR = gR.add(vs[i].getLowBound());
            }
        }
        LinearFunction lte = new LinearFunction(lIcs, vs, 0);
        LinearFunction gte = new LinearFunction(gIcs, vs, 0);
        switch (equation.getOp()) {
            case Equal:
                return new LinearEquation[]{
                    new LinearEquation(lte, Op.LessThanEqual, lR.ceil(), false),
                    new LinearEquation(gte, Op.LessThanEqual, -gR.floor(), false)};
            case LessThanEqual:
                return new LinearEquation[]{new LinearEquation(lte, Op.LessThanEqual, lR.ceil())};
            default:
                throw new IllegalStateException();
        }
    }

    private static IrBoolExpr[] boolExpr(LinearEquation equation, Map<Variable, IrIntVar> map) {
        LinearEquation[] rounds = round(equation);
        IrBoolExpr[] exprs = new IrBoolExpr[rounds.length];
        for (int i = 0; i < exprs.length; i++) {
            LinearEquation round = rounds[i];
            LinearFunction left = round.getLeft();
            Rational[] cs = left.getCoefficients();
            Variable[] vs = left.getVariables();
            Rational right = round.getRight();

            IrIntExpr[] addends = new IrIntExpr[cs.length];
            for (int j = 0; j < addends.length; j++) {
                assert cs[j].isWhole();
                addends[j] = mul((int) cs[j].getNumerator(), map.get(vs[j]), Domains.Unbounded);
            }
            assert right.isWhole();
            switch (round.getOp()) {
                case Equal:
                    exprs[i] = equal(add(addends), (int) right.getNumerator());
                    break;
                case LessThanEqual:
                    exprs[i] = lessThanEqual(add(addends), (int) right.getNumerator());
                    break;
                default:
                    throw new IllegalStateException();
            }
        }
        return exprs;
    }

    public static IrModule optimize(IrModule module) {
        List<IrBoolExpr> constraints = new ArrayList<>();
        Set<LinearEquation> equations = new HashSet<>();
        Map<IrIntVar, Variable> map = new HashMap<>();
        for (IrBoolExpr constraint : module.getConstraints()) {
            LinearEquation equation = linearEquation(constraint, map);
            if (equation != null) {
                equations.add(equation);
            } else {
                constraints.add(constraint);
            }
        }

        if (equations.size() > 0) {
            Map<Variable, IrIntVar> inverse = Util.inverse(map);

            LinearSystem system = new LinearSystem(equations);
            for (LinearEquation equation : system
                    .equalityElimination()
                    .fourierMotzkinElimination()
                    .strengthenInequalities()
                    .gaussJordanElimination()
                    .addEquations(equations)
                    .dominantElimination()
                    .getEquations()) {
                for (IrBoolExpr constraint : boolExpr(equation, inverse)) {
                    constraints.add(constraint);
                }
            }
            return new IrModule().addConstraints(constraints);
        }
        return module;
    }
}
