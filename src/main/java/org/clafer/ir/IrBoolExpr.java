package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrBoolExpr {

    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a);
}
