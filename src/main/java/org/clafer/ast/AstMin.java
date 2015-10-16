package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstMin implements AstSetExpr {

    private final AstSetExpr set;

    public AstMin(AstSetExpr set) {
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
        if (obj instanceof AstMin) {
            AstMin other = (AstMin) obj;
            return set.equals(other.set);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 207 ^ set.hashCode();
    }

    @Override
    public String toString() {
        return "min(" + set + ")";
    }
}
