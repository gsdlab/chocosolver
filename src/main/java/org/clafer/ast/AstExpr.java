package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public interface AstExpr {

    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a);
}
