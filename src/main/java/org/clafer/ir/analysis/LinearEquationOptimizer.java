package org.clafer.ir.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
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
import org.clafer.math.LinearFunction;
import org.clafer.math.LinearFunctionBuilder;
import org.clafer.math.LinearSystem;
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
                builder.plusFunction(mulBuilder.toFunction().scale(coefficient));
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
                    .fourierMotzkinElimination()
                    .gaussJordanElimination()
                    .dominantElimination()
                    .getEquations()) {
                LinearFunction left = equation.getLeft();
                int[] coefficients = left.getCoefficients();
                Variable[] variables = left.getVariables();
                int right = equation.getRight();
                List<IrIntExpr> addends = new ArrayList<>(coefficients.length);
                for (int i = 0; i < coefficients.length; i++) {
                    addends.add(mul(constant(coefficients[i]), inverse.get(variables[i])));
                }
                switch (equation.getOp()) {
                    case Equal:
                        constraints.add(equal(add(addends), right));
                        break;
                    case LessThanEqual:
                        constraints.add(lessThanEqual(add(addends), right));
                        break;
                    default:
                        throw new IllegalStateException();
                }
            }
            return new IrModule().addConstraints(constraints);
        }
        return module;
    }
}
