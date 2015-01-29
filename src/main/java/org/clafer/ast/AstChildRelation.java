package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstChildRelation implements AstSetExpr {

    private final AstConcreteClafer childRelation;

    AstChildRelation(AstConcreteClafer childRelation) {
        this.childRelation = Check.notNull(childRelation);
    }

    public AstConcreteClafer getChildType() {
        return childRelation;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstChildRelation) {
            AstChildRelation other = (AstChildRelation) obj;
            return childRelation.equals(other.childRelation);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 1311 * childRelation.hashCode();
    }

    @Override
    public String toString() {
        return childRelation.toString();
    }
}
