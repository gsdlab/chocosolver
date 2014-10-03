package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstTransitiveClosure implements AstSetExpr {

    private final AstSetExpr relation;

    AstTransitiveClosure(AstSetExpr relation) {
        this.relation = Check.notNull(relation);
    }

    public AstSetExpr getRelation() {
        return relation;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstTransitiveClosure) {
            AstTransitiveClosure other = (AstTransitiveClosure) obj;
            return relation.equals(other.relation);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 83 * relation.hashCode();
    }

    @Override
    public String toString() {
        return relation + "*";
    }
}
