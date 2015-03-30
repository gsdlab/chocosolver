package org.clafer.ir;

/**
 * Dynamic dispatch for IR integer expressions.
 *
 * @param <A> the parameter type
 * @param <B> the return type
 * @author jimmy
 */
public interface IrIntExprVisitor<A, B> extends IrBoolExprVisitor<A, B> {

    public B visit(IrIntVar ir, A a);

    public B visit(IrMinus ir, A a);

    public B visit(IrCard ir, A a);

    public B visit(IrAdd ir, A a);

    public B visit(IrMul ir, A a);

    public B visit(IrDiv ir, A a);

    public B visit(IrMod ir, A a);

    public B visit(IrElement ir, A a);

    public B visit(IrCount ir, A a);

    public B visit(IrCountNotEqual ir, A a);

    public B visit(IrSetMax ir, A a);

    public B visit(IrSetSum ir, A a);

    public B visit(IrTernary ir, A a);

    public B visit(IrLength ir, A a);
}
