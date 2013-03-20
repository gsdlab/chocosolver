package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public interface AstExpression {

    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a);
}
