package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstDowncast(AstSetExpr base, ProductType target) implements AstSetExpr {

    public AstDowncast {
        base = Check.notNull(base);
        target = Check.notNull(target);
    }

    @Override
    public String toString() {
        return "(" + target + ") " + base;
    }
}
