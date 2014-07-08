package org.clafer.ir;

import org.clafer.domain.BoolDomain;

/**
 * An immutable expression that evaluates to a boolean.
 *
 * @author jimmy
 */
public interface IrBoolExpr extends IrIntExpr {

    /**
     * @return the domain of values this expression can take
     */
    @Override
    public BoolDomain getDomain();

    /**
     * The negated expression is true if and only if this expression is false.
     *
     * @return negated expression
     */
    public IrBoolExpr negate();

    /**
     * Is the expression in negative form. Expressions in negated form have
     * their class names prefixed with "IrNot...", otherwise the expression is
     * not negative.
     *
     * @return true if the expression is in negative form, false otherwise
     */
    public boolean isNegative();

    /**
     * Dynamic dispatch on the visitor.
     *
     * @param <A> the parameter type
     * @param <B> the return type
     * @param visitor the visitor
     * @param a the parameter
     * @return the return value
     */
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a);
}
