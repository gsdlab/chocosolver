package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstCard(AstSetExpr set) implements AstSetExpr {

    public AstCard {
        set = Check.notNull(set);
    }

    @Override
    public String toString() {
        return "#(" + set + ")";
    }
}
