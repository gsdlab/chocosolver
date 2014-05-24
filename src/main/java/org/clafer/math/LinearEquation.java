package org.clafer.math;

import org.clafer.common.Check;
import org.clafer.domain.BoolDomain;

/**
 * @author jimmy
 */
public class LinearEquation {

    private final LinearFunction left;
    private final Op op;
    private final int right;

    public LinearEquation(LinearFunction left, Op op, int right) {
        Check.notNull(left);
        Check.notNull(op);
        Check.notNull(right);
        if (left.hasConstant()) {
            throw new IllegalArgumentException();
        }
        this.left = left;
        this.op = op;
        this.right = right;
    }

    public static LinearEquation equal(
            LinearFunction left, LinearFunction right) {
        LinearFunction combine = left.sub(right);
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.Equal,
                -combine.getConstant());
    }

    public static LinearEquation equal(
            LinearFunctionBuilder left, LinearFunctionBuilder right) {
        return equal(left.toFunction(), right.toFunction());
    }

    public static LinearEquation lessThanEqual(
            LinearFunction left, LinearFunction right) {
        LinearFunction combine = left.sub(right);
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.LessThanEqual,
                -combine.getConstant());
    }

    public static LinearEquation lessThanEqual(
            LinearFunctionBuilder left, LinearFunctionBuilder right) {
        return lessThanEqual(left.toFunction(), right.toFunction());
    }

    public static LinearEquation greaterThanEqual(
            LinearFunction left, LinearFunction right) {
        return lessThanEqual(right, left);
    }

    public static LinearEquation greaterThanEqual(
            LinearFunctionBuilder left, LinearFunctionBuilder right) {
        return greaterThanEqual(left.toFunction(), right.toFunction());
    }

    public LinearFunction getLeft() {
        return left;
    }

    public Op getOp() {
        return op;
    }

    public int getRight() {
        return right;
    }

    public BoolDomain isEntailed() {
        switch (op) {
            case Equal:
                if (left.getLowBound() > right || left.getHighBound() < right) {
                    return BoolDomain.FalseDomain;
                }
                if (left.getLowBound() == right && left.getHighBound() == right) {
                    return BoolDomain.TrueDomain;
                }
                return BoolDomain.TrueFalseDomain;
            case LessThanEqual:
                if (left.getLowBound() > right) {
                    return BoolDomain.FalseDomain;
                }
                if (left.getHighBound() <= right) {
                    return BoolDomain.TrueDomain;
                }
                return BoolDomain.TrueFalseDomain;
            default:
                throw new IllegalStateException();
        }
    }

//    public int getLowBound(Variable variable) {
//        int low = right;
//        int[] coefficients = left.getCoefficients();
//        Variable[] variables = left.getVariables();
//        assert Util.in(variable, variables);
//        int ai = 0;
//        for (int i = 0; i < coefficients.length; i++) {
//            if (!variable.equals(variables[i])) {
//                low -= coefficients[i] * (coefficients[i] > 0
//                        ? variables[i].getHighBound()
//                        : variables[i].getLowBound());
//                System.out.println(variables[i] + " : " + low);
//            } else {
//                ai = coefficients[i];
//            }
//        }
//        return MathUtils.divCeil(low, ai);
//    }
//
//    public int getHighBound(Variable variable) {
//        int high = right;
//        int[] coefficients = left.getCoefficients();
//        Variable[] variables = left.getVariables();
//        assert Util.in(variable, variables);
//        int ai = 0;
//        for (int i = 0; i < coefficients.length; i++) {
//            if (!variable.equals(variables[i])) {
//                high -= coefficients[i] * (coefficients[i] > 0
//                        ? variables[i].getLowBound()
//                        : variables[i].getHighBound());
//            } else {
//                ai = coefficients[i];
//            }
//        }
//        return MathUtils.divFloor(high, ai);
//    }
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof LinearEquation) {
            LinearEquation other = (LinearEquation) obj;
            return right == other.right && op.equals(other.op) && left.equals(other.left);
        }
        return false;
    }

    @Override
    public int hashCode() {
        // op.hashCode() can change between runs which makes the output change
        // every time.
        return left.hashCode() ^ op.ordinal() ^ right;
    }

    @Override
    public String toString() {
        return left + " " + op + " " + right;
    }

    public static enum Op {

        Equal("="),
        LessThanEqual("<=");

        private final String syntax;

        Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }

        @Override
        public String toString() {
            return syntax;
        }
    }
}
