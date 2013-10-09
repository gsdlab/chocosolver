package org.clafer.ir;

/**
 * An immutable expression that evaluates to an integer.
 *
 * @author jimmy
 */
public interface IrIntExpr extends IrExpr {

    /**
     * Domain cannot be empty.
     *
     * @return the domain of values this expression can take
     */
    public IrDomain getDomain();

    /**
     * Dynamic dispatch on the visitor.
     *
     * @param <A> the parameter type
     * @param <B> the return type
     * @param visitor the visitor
     * @param a the parameter
     * @return the return value
     */
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a);
}
