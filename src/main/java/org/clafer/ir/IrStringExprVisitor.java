package org.clafer.ir;

/**
 * Dynamic dispatch for IR string expressions.
 *
 * @param <A> the parameter type
 * @param <B> the return type
 * @author jimmy
 */
public interface IrStringExprVisitor<A, B> {

    public B visit(IrStringVar ir, A a);

    public B visit(IrStringElement ir, A a);

    public B visit(IrConcat ir, A a);
}
