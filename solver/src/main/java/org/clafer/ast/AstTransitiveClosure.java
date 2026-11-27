package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstTransitiveClosure(AstSetExpr relation, boolean reflexive) implements AstSetExpr {

    public AstTransitiveClosure {
        relation = Check.notNull(relation);
    }

    @Override
    public String toString() {
        return relation + (reflexive ? "**" : "*");
    }
}
