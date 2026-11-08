package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstConnected(AstSetExpr nodes, AstSetExpr relation, boolean directed) implements AstBoolExpr {

    public AstConnected {
        nodes = Check.notNull(nodes);
        relation = Check.notNull(relation);
    }

    @Override
    public String toString() {
        return "connected(" + nodes + ", " + relation + ")" + (directed ? "**" : "*");
    }
}
