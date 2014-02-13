package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetTest extends IrAbstractBool {

    private final IrSetExpr left;
    private final Op op;
    private final IrSetExpr right;

    IrSetTest(IrSetExpr left, Op op, IrSetExpr right, IrBoolDomain domain) {
        super(domain);
        this.left = Check.notNull(left);
        this.op = Check.notNull(op);
        this.right = Check.notNull(right);
    }

    public IrSetExpr getLeft() {
        return left;
    }

    public Op getOp() {
        return op;
    }

    public IrSetExpr getRight() {
        return right;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrSetTest(left, op.negate(), right, getDomain().invert());
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
        if (obj instanceof IrSetTest) {
            IrSetTest other = (IrSetTest) obj;
            return left.equals(other.left) && op.equals(other.op) && right.equals(other.right) && super.equals(other);
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
        NotEqual("!=");
        private final String syntax;

        Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }

        public Op negate() {
            switch (this) {
                case Equal:
                    return NotEqual;
                case NotEqual:
                    return Equal;
                default:
                    throw new IllegalStateException();
            }
        }
    }
}
