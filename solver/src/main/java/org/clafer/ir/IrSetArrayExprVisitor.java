package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrSetArrayExprVisitor<A, B> {

    public B visit(IrSetArrayVar ir, A a);

    public B visit(IrInverse ir, A a);

    public B visit(IrTransitiveClosure ir, A a);
}
