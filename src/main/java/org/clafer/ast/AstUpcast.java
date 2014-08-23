package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstUpcast implements AstSetExpr {

    private final AstSetExpr base;
    private final ProductType target;

    AstUpcast(AstSetExpr base, ProductType target) {
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
        if (obj instanceof AstUpcast) {
            AstUpcast other = (AstUpcast) obj;
            return base.equals(other.base) && target.equals(other.target);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 203 ^ base.hashCode() ^ target.hashCode();
    }

    @Override
    public String toString() {
        return "(" + target + ") " + base;
    }
}
