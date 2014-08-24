package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstParentRelation implements AstSetExpr {

    private final AstConcreteClafer parentRelation;

    AstParentRelation(AstConcreteClafer parentRelation) {
        this.parentRelation = Check.notNull(parentRelation);
    }

    public AstConcreteClafer getParentRelation() {
        return parentRelation;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
