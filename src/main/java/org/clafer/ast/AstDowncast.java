package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstDowncast implements AstSetExpr {

    private final AstSetExpr base;
    private final ProductType target;

    AstDowncast(AstSetExpr base, ProductType target) {
        this.base = Check.notNull(base);
        this.target = Check.notNull(target);
    }

    public AstSetExpr getBase() {
        return base;
    }

    public ProductType getTarget() {
        return target;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstDowncast) {
            AstDowncast other = (AstDowncast) obj;
            return base.equals(other.base) && target.equals(other.target);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 207 ^ base.hashCode() ^ target.hashCode();
    }

    @Override
    public String toString() {
        return "(" + target + ") " + base;
    }
}
