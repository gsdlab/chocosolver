package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstDomainRestriction(AstSetExpr domain, AstSetExpr relation) implements AstSetExpr {

    public AstDomainRestriction {
        domain = Check.notNull(domain);
        relation = Check.notNull(relation);
    }

    @Override
    public String toString() {
        return domain + " <: " + relation;
    }
}
