package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstNot implements AstBoolExpr {

    private final AstBoolExpr expr;

    AstNot(AstBoolExpr expr) {
        this.expr = Check.notNull(expr);
    }

    public AstBoolExpr getExpr() {
        return expr;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "!(" + expr + ")";
    }
}
