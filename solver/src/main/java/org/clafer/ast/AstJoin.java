package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstJoin(AstSetExpr left, AstSetExpr right) implements AstSetExpr {

    public AstJoin {
        left = Check.notNull(left);
        right = Check.notNull(right);
    }

    @Override
    public String toString() {
        return left + " . " + right;
    }
}
