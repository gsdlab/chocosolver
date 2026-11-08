package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstUpcast(AstSetExpr base, ProductType target) implements AstSetExpr {

    public AstUpcast {
        base = Check.notNull(base);
        target = Check.notNull(target);
    }

    @Override
    public String toString() {
        return "(" + target + ") " + base;
    }
}
