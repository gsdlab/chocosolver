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
        if (Op.Equal.equals(op) && mostlyNegative(left.getCoefficients())) {
            this.left = left.scale(-1);
            this.right = -right;
        } else {
            this.left = left;
            this.right = right;
        }
        this.op = op;
    }

    private static boolean mostlyNegative(int[] is) {
        int negative = 0;
        for (int i : is) {
            if (i < 0) {
                negative++;
            }
        }
        return negative * 2 > is.length;
    }

    public static LinearEquation equal(LinearFunctionable left, LinearFunctionable right) {
        LinearFunction combine = left.toFunction().sub(right.toFunction());
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.Equal,
                -combine.getConstant());
    }

    public static LinearEquation lessThan(LinearFunctionable left, LinearFunctionable right) {
        LinearFunction combine = left.toFunction().sub(right.toFunction());
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.LessThanEqual,
                -combine.getConstant() - 1);
    }

    public static LinearEquation lessThanEqual(LinearFunctionable left, LinearFunctionable right) {
        LinearFunction combine = left.toFunction().sub(right.toFunction());
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.LessThanEqual,
                -combine.getConstant());
    }

    public static LinearEquation greaterThan(LinearFunctionable left, LinearFunctionable right) {
        return lessThan(right, left);
    }

    public static LinearEquation greaterThanEqual(LinearFunctionable left, LinearFunctionable right) {
        return lessThanEqual(right, left);
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
