package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrIntExprVisitor<A, B> {

    public B visit(IrIntVar ir, A a);

    public B visit(IrCard ir, A a);

    public B visit(IrArithm ir, A a);

    public B visit(IrSum ir, A a);

    public B visit(IrElement ir, A a);
}
