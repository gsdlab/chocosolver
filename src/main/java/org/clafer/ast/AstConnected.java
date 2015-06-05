package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstConnected implements AstBoolExpr {

    private final AstSetExpr relation;
    private final AstSetExpr nodes;
    private final boolean directed;

    AstConnected(AstSetExpr nodes, AstSetExpr relation, boolean directed) {
        this.directed = directed;
        this.nodes = Check.notNull(nodes);
        this.relation = Check.notNull(relation);
    }

    public AstSetExpr getRelation() {
        return relation;
    }

    public AstSetExpr getNodes() {
        return nodes;
    }

    public boolean isDirected() {
        return directed;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstConnected) {
            AstConnected other = (AstConnected) obj;
            return relation.equals(other.relation) && directed == other.directed;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 83 * relation.hashCode() ^ (directed ? 8 : 0);
    }

    @Override
    public String toString() {
        return relation + (directed ? "**" : "*");
    }
}
