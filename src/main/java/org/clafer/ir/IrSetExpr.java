package org.clafer.ir;

/**
 * An immutable expression that evaluates to a set of integers.
 * 
 * @author jimmy
 */
public interface IrSetExpr extends IrSet, IrExpr {

    /**
     * Visitor pattern.
     */
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a);
}
