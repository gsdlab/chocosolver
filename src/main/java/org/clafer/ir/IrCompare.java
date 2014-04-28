package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 * left `op` right + offset
 *
 * @author jimmy
 */
public class IrCompare extends IrAbstractBool {

    private final IrIntExpr left;
    private final Op op;
    private final IrIntExpr right;

    IrCompare(IrIntExpr left, Op op, IrIntExpr right, BoolDomain domain) {
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
        switch (op) {
            case Equal:
                return new IrCompare(left, Op.NotEqual, right, getDomain().invert());
            case NotEqual:
                return new IrCompare(left, Op.Equal, right, getDomain().invert());
            case LessThan:
                return new IrCompare(right, Op.LessThanEqual, left, getDomain().invert());
            case LessThanEqual:
                return new IrCompare(right, Op.LessThan, left, getDomain().invert());
            default:
                throw new IllegalStateException();
        }
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
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrCompare) {
            IrCompare other = (IrCompare) obj;
            return left.equals(other.left) && op.equals(other.op)
                    && right.equals(other.right) && super.equals(other);
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
        return left + " " + op.getSyntax() + " " + right;
    }

    public static enum Op {

        Equal("="),
        NotEqual("!="),
        LessThan("<"),
        LessThanEqual("<=");
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
    }
}
