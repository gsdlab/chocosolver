package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSetEquality implements IrBoolExpr {

    private final IrSetExpr left;
    private final Op op;
    private final IrSetExpr right;

    IrSetEquality(IrSetExpr left, Op op, IrSetExpr right) {
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
    public IrBoolExpr opposite() {
        return new IrSetEquality(left, op.getOpposite(), right);
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
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

        public Op getOpposite() {
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
