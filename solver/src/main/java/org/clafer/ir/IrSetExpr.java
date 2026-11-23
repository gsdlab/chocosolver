package org.clafer.ir;

import org.clafer.domain.Domain;

/**
 * An immutable expression that evaluates to a set of integers.
 *
 * @author jimmy
 */
public interface IrSetExpr extends IrExpr {

    /**
     * Env or envelope is the union of all possible values the set can be.
     *
     * @return the env domain
     */
    public Domain getEnv();

    /**
     * Ker or kernel is the intersection of all possible values the set can be
     *
     * @return the ker domain
     */
    public Domain getKer();

    /**
     * Card or cardinality is all the possible size of the set.
     *
     * @return the card domain
     */
    public Domain getCard();

    /**
     * Dynamic dispatch on the visitor.
     *
     * @param <A> the parameter type
     * @param <B> the return type
     * @param visitor the visitor
     * @param a the parameter
     * @return the return value
     */
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a);
}
