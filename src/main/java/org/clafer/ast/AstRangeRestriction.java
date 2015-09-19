package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstRangeRestriction implements AstSetExpr {

    private final AstSetExpr relation, range;

    AstRangeRestriction(AstSetExpr relation, AstSetExpr set) {
        this.relation = Check.notNull(relation);
        this.range = Check.notNull(set);
    }

    public AstSetExpr getRelation() {
        return relation;
    }

    public AstSetExpr getRange() {
        return range;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstRangeRestriction) {
            AstRangeRestriction other = (AstRangeRestriction) obj;
            return relation.equals(other.relation) && range.equals(other.range);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 193 * relation.hashCode() ^ range.hashCode();
    }

    @Override
    public String toString() {
        return relation + " :> " + range;
    }
}
