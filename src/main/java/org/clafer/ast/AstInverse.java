package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstInverse implements AstSetExpr {

    private final AstSetExpr relation;

    AstInverse(AstSetExpr relation) {
        this.relation = Check.notNull(relation);
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
        if (obj instanceof AstInverse) {
            AstInverse other = (AstInverse) obj;
            return relation.equals(other.relation);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 93 * relation.hashCode();
    }

    @Override
    public String toString() {
        return "~" + relation;
    }
}
