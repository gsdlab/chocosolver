package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstProduct(AstSetExpr set) implements AstSetExpr {

    public AstProduct {
        set = Check.notNull(set);
    }

    @Override
    public String toString() {
        return "product(" + set + ")";
    }
}
