package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstJoinParent(AstSetExpr children) implements AstSetExpr {

    public AstJoinParent {
        children = Check.notNull(children);
    }

    @Override
    public String toString() {
        return children + " . parent";
    }
}
