package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstIntersection(AstSetExpr left, AstSetExpr right) implements AstSetExpr {

    public AstIntersection {
        left = Check.notNull(left);
        right = Check.notNull(right);
    }

    @Override
    public String toString() {
        return "(" + left + ") & (" + right + ")";
    }
}
