package org.clafer.ast;

import org.clafer.common.Check;

/**
 * Negate a boolean expression.
 *
 * @author jimmy
 */
public class AstNot implements AstBoolExpr {

    private final AstBoolExpr expr;

    AstNot(AstBoolExpr expr) {
        this.expr = Check.notNull(expr);
    }

    /**
     * Returns the expression to negated.
     * 
     * @return the expression to negate
     */
    public AstBoolExpr getExpr() {
        return expr;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return "!(" + expr + ")";
    }
}
