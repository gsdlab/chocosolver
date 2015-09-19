package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstDomainRestriction implements AstSetExpr {

    private final AstSetExpr domain, relation;

    AstDomainRestriction(AstSetExpr domain, AstSetExpr relation) {
        this.domain = Check.notNull(domain);
        this.relation = Check.notNull(relation);
    }

    public AstSetExpr getDomain() {
        return domain;
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
        if (obj instanceof AstDomainRestriction) {
            AstDomainRestriction other = (AstDomainRestriction) obj;
            return domain.equals(other.domain) && relation.equals(other.relation);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 393 * domain.hashCode() ^ relation.hashCode();
    }

    @Override
    public String toString() {
        return domain + " <: " + relation;
    }
}
