package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstJoinParent implements AstSetExpression {

    private final AstSetExpression children;

    public AstJoinParent(AstSetExpression children) {
        this.children = Check.notNull(children);
    }

    public AstSetExpression getChildren() {
        return children;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
