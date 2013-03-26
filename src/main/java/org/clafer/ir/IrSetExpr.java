package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrSetExpr {

    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a);
}
