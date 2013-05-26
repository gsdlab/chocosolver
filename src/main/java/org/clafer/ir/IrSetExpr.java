package org.clafer.ir;

/**
 * An immutable expression that evaluates to a set of integers.
 *
 * @author jimmy
 */
public interface IrSetExpr extends IrSet, IrExpr {

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
