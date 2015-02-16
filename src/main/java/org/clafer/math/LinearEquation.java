package org.clafer.math;

import org.clafer.common.Check;
import org.clafer.common.Util;
import org.clafer.domain.BoolDomain;

/**
 * @author jimmy
 */
public class LinearEquation {

    private final LinearFunction left;
    private final Op op;
    private final Rational right;

    public LinearEquation(LinearFunction left, Op op, long right) {
        this(left, op, new Rational(right));
    }

    public LinearEquation(LinearFunction left, Op op, Rational right) {
        this(left, op, right, true);
    }

    public LinearEquation(LinearFunction left, Op op, long right, boolean scale) {
        this(left, op, new Rational(right), scale);
    }

    public LinearEquation(LinearFunction left, Op op, Rational right, boolean scale) {
        Check.notNull(left);
        Check.notNull(op);
        Check.notNull(right);
        if (left.hasConstant()) {
            throw new IllegalArgumentException();
        }
        if (!scale || right.isZero()) {
            this.left = left;
            this.right = right;
        } else {
            this.left = left.div(right.abs());
            this.right = right.div(right.abs());
        }
        this.op = op;
    }

    public static LinearEquation equal(LinearFunctionable left, LinearFunctionable right) {
        LinearFunction combine = left.toFunction().sub(right.toFunction());
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.Equal,
                combine.getConstant().minus());
    }

    public static LinearEquation lessThan(LinearFunctionable left, LinearFunctionable right) {
        LinearFunction combine = left.toFunction().sub(right.toFunction());
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.LessThanEqual,
                combine.getConstant().minus().sub(Rational.One));
    }

    public static LinearEquation lessThanEqual(LinearFunctionable left, LinearFunctionable right) {
        LinearFunction combine = left.toFunction().sub(right.toFunction());
        return new LinearEquation(
                combine.sub(combine.getConstant()),
                Op.LessThanEqual,
                combine.getConstant().minus());
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

    public Rational getRight() {
        return right;
    }

    public Variable[] getVariables() {
        return left.getVariables();
    }

    public LinearEquation replace(Variable variable, LinearFunction value) {
        LinearFunction newLeft = left.replace(variable, value);
        return new LinearEquation(
                newLeft.sub(newLeft.getConstant()),
                op,
                right.sub(newLeft.getConstant()));
    }

    public LinearEquation normalize() {
        long denominator = right.getDenominator();
        for (Rational coefficient : left.getCoefficients()) {
            denominator = Util.lcm(denominator, coefficient.getDenominator());
        }
        assert denominator > 0;
        return new LinearEquation(left.mul(denominator), op, right.mul(denominator));
    }

    public BoolDomain isEntailed() {
        switch (op) {
            case Equal:
                if (left.getLowBound().compareTo(right) > 0 || left.getHighBound().compareTo(right) < 0) {
                    return BoolDomain.FalseDomain;
                }
                if (left.getLowBound().equals(right) && left.getHighBound().equals(right)) {
                    return BoolDomain.TrueDomain;
                }
                return BoolDomain.TrueFalseDomain;
            case LessThanEqual:
                if (left.getLowBound().compareTo(right) > 0) {
                    return BoolDomain.FalseDomain;
                }
                if (left.getHighBound().compareTo(right) <= 0) {
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
            return op.equals(other.op) && left.equals(other.left) && right.equals(other.right);
        }
        return false;
    }

    @Override
    public int hashCode() {
        // op.hashCode() can change between runs which makes the output change
        // every time.
        return left.hashCode() ^ op.ordinal() ^ right.hashCode();
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
