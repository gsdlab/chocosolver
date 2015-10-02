package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstJoinRef implements AstSetExpr {

    private final AstSetExpr deref;

    AstJoinRef(AstSetExpr deref) {
        this.deref = Check.notNull(deref);
    }

    public AstSetExpr getDeref() {
        return deref;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstJoinRef) {
            AstJoinRef other = (AstJoinRef) obj;
            return deref.equals(other.deref);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return -133 ^ deref.hashCode();
    }

    @Override
    public String toString() {
        return deref + " . ref";
    }
}
