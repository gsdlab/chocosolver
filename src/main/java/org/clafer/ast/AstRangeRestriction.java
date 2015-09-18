package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstRangeRestriction implements AstSetExpr {

    private final AstSetExpr relation, set;

    AstRangeRestriction(AstSetExpr relation, AstSetExpr set) {
        this.relation = Check.notNull(relation);
        this.set = Check.notNull(set);
    }

    public AstSetExpr getRelation() {
        return relation;
    }

    public AstSetExpr getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstRangeRestriction) {
            AstRangeRestriction other = (AstRangeRestriction) obj;
            return relation.equals(other.relation) && set.equals(other.set);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 193 * relation.hashCode() ^ set.hashCode();
    }

    @Override
    public String toString() {
        return relation + " :> " + set;
    }
}
