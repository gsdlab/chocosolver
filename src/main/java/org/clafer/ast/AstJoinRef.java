package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstJoinRef implements AstSetExpression {

    private final AstSetExpression deref;

    AstJoinRef(AstSetExpression deref) {
        this.deref = Check.notNull(deref);
    }

    public AstSetExpression getDeref() {
        return deref;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
