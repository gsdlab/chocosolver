package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstRangeRestriction(AstSetExpr relation, AstSetExpr range) implements AstSetExpr {

    public AstRangeRestriction {
        relation = Check.notNull(relation);
        range = Check.notNull(range);
    }

    @Override
    public String toString() {
        return relation + " :> " + range;
    }
}
