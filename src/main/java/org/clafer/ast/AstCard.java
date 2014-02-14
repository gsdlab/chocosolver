package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstCard implements AstSetExpr {

    private final AstSetExpr set;

    public AstCard(AstSetExpr set) {
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
        if (obj instanceof AstCard) {
            AstCard other = (AstCard) obj;
            return set.equals(other.set);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 187 ^ set.hashCode();
    }

    @Override
    public String toString() {
        return "#(" + set + ")";
    }
}
