package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstLocal implements AstSetExpression {

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
