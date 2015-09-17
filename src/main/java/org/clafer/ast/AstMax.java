package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstMax implements AstSetExpr {

    private final AstSetExpr set;

    public AstMax(AstSetExpr set) {
        this.set = Check.notNull(set);
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
        if (obj instanceof AstMax) {
            AstMax other = (AstMax) obj;
            return set.equals(other.set);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 203 ^ set.hashCode();
    }

    @Override
    public String toString() {
        return "max(" + set + ")";
    }
}
