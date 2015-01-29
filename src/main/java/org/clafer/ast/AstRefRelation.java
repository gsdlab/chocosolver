package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstRefRelation implements AstSetExpr {

    private final AstRef refRelation;

    public AstRefRelation(AstRef refRelation) {
        this.refRelation = Check.notNull(refRelation);
    }

    public AstRef getRef() {
        return refRelation;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
