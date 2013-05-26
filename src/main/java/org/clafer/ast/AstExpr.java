package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public interface AstExpr {

    /**
     * Dynamic dispatch on the visitor.
     *
     * @param <A> the parameter type
     * @param <B> the return type
     * @param visitor the visitor
     * @param a the parameter
     * @return the return value
     */
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a);
}
