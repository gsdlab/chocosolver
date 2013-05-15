package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrBoolExpr extends IrExpr {

    public IrBoolExpr opposite();

    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a);
}
