package org.clafer.ir;

import org.clafer.common.Check;

/**
 * left `op` right + offset
 *
 * @author jimmy
 */
public class IrCompare extends IrAbstractBool implements IrBoolExpr {

    private final IrIntExpr left;
    private final Op op;
    private final IrIntExpr right;

    IrCompare(IrIntExpr left, Op op, IrIntExpr right, IrBoolDomain domain) {
        super(domain);
        this.left = Check.notNull(left);
        this.op = Check.notNull(op);
        this.right = Check.notNull(right);
    }

    public IrIntExpr getLeft() {
        return left;
    }

    public Op getOp() {
        return op;
    }

    public IrIntExpr getRight() {
        return right;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrCompare(left, op.negate(), right, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrCompare) {
            IrCompare other = (IrCompare) obj;
            return left.equals(other.left) && op.equals(other.op) && right.equals(other.right) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return left.hashCode() ^ op.hashCode() ^ right.hashCode();
    }

    @Override
    public String toString() {
        return left + " " + op.getSyntax() + " " + right;
    }

    public static enum Op {

        Equal("="),
        NotEqual("!="),
        LessThan("<"),
        LessThanEqual("<="),
        GreaterThan(">"),
        GreaterThanEqual(">=");
        private final String syntax;

        Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }

        public boolean isEquality() {
            switch (this) {
                case Equal:
                case NotEqual:
                    return true;
                default:
                    return false;
            }
        }

        public boolean isInequality() {
            return !isEquality();
        }

        public Op reverse() {
            switch (this) {
                case Equal:
                    return Equal;
                case NotEqual:
                    return NotEqual;
                case LessThan:
                    return GreaterThan;
                case LessThanEqual:
                    return GreaterThanEqual;
                case GreaterThan:
                    return LessThan;
                case GreaterThanEqual:
                    return LessThanEqual;
                default:
                    throw new IllegalStateException();
            }
        }

        public Op negate() {
            switch (this) {
                case Equal:
                    return NotEqual;
                case NotEqual:
                    return Equal;
                case LessThan:
                    return GreaterThanEqual;
                case LessThanEqual:
                    return GreaterThan;
                case GreaterThan:
                    return LessThanEqual;
                case GreaterThanEqual:
                    return LessThan;
                default:
                    throw new IllegalStateException();
            }
        }
    }
}
