package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstInverse(AstSetExpr relation) implements AstSetExpr {

    public AstInverse {
        relation = Check.notNull(relation);
    }

    @Override
    public String toString() {
        return "~" + relation;
    }
}
