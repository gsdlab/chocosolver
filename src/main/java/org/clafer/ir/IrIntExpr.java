package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrIntExpr extends IrExpr {

    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a);
}
