package org.clafer.ast;

/**
 * Do NOT reuse for different expressions.
 * 
 * @author jimmy
 */
public class AstThis implements AstSetExpression {

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
