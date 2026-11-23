package org.clafer.ir;

import org.clafer.domain.Domain;

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
    public Domain getDomain();

    /**
     * @return {@code getDomain().getLowBound()}
     */
    public int getLowBound();

    /**
     * @return {@code getDomain().getHighBound()}
     */
    public int getHighBound();

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
