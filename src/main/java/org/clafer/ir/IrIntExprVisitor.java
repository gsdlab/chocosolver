package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrIntExprVisitor<A, B> {

    public B visit(IrIntLiteral ir, A a);

    public B visit(IrCard ir, A a);

    public B visit(IrAdd ir, A a);

    public B visit(IrSub ir, A a);

    public B visit(IrMul ir, A a);

    public B visit(IrDiv ir, A a);

    public B visit(IrElement ir, A a);
}
