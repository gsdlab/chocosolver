package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstJoinRef(AstSetExpr deref) implements AstSetExpr {

    public AstJoinRef {
        deref = Check.notNull(deref);
    }

    @Override
    public String toString() {
        return deref + " . ref";
    }
}
