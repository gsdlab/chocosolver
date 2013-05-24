package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstMinus implements AstSetExpr {

    private final AstSetExpr expr;

    AstMinus(AstSetExpr expr) {
        this.expr = expr;
    }

    public AstSetExpr getExpr() {
        return expr;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "-(" + expr + ")";
    }
}
