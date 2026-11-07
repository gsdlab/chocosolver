package org.clafer.ast;

import org.clafer.common.Check;

/**
 * Negate a boolean expression.
 *
 * @author jimmy
 */
public record AstNot(AstBoolExpr expr) implements AstBoolExpr {

    public AstNot {
        expr = Check.notNull(expr);
    }

    @Override
    public String toString() {
        return "!(" + expr + ")";
    }
}
