package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstMin(AstSetExpr set) implements AstSetExpr {

    public AstMin {
        set = Check.notNull(set);
    }

    @Override
    public String toString() {
        return "min(" + set + ")";
    }
}
