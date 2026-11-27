package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstUnion(AstSetExpr left, AstSetExpr right) implements AstSetExpr {

    public AstUnion {
        left = Check.notNull(left);
        right = Check.notNull(right);
    }

    @Override
    public String toString() {
        return "(" + left + ") ++ (" + right + ")";
    }
}
