package org.clafer.ir;

/**
 * An immutable expression that evaluates to an integer.
 * 
 * @author jimmy
 */
public interface IrIntExpr extends IrInt, IrExpr {

    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a);
}
