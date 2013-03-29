package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrArithm implements IrIntExpr {

    private final IrIntExpr left;
    private final Op op;
    private final IrIntExpr right;

    IrArithm(IrIntExpr left, Op op, IrIntExpr right) {
        this.left = Check.notNull(left);
        this.op = Check.notNull(op);
        this.right = Check.notNull(right);
    }

    public IrIntExpr getLeft() {
        return left;
    }

    public IrIntExpr getRight() {
        return right;
    }

    public Op getOp() {
        return op;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return left + " " + op.getSyntax() + " " + right;
    }

    public static enum Op {

        Add("+"),
        Sub("-"),
        Mul("*"),
        Div("/");
        private final String syntax;

        Op(String syntax) {
            this.syntax = syntax;
        }

        public String getSyntax() {
            return syntax;
        }
    }
}
