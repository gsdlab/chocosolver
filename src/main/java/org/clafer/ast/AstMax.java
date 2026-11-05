package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstMax(AstSetExpr set) implements AstSetExpr {

    public AstMax {
        set = Check.notNull(set);
    }

    @Override
    public String toString() {
        return "max(" + set + ")";
    }
}
