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
}
