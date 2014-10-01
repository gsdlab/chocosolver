package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrIntArrayExprVisitor<A, B> {

    public B visit(IrIntArrayVar ir, A a);
}
