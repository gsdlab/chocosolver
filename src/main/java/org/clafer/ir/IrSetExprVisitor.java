package org.clafer.ir;

/**
 * Dynamic dispatch for IR set expressions.
 *
 * @param <A> the parameter type
 * @param <B> the return type
 * @author jimmy
 */
public interface IrSetExprVisitor<A, B> {

    public B visit(IrSetVar ir, A a);

    public B visit(IrSingleton ir, A a);

    public B visit(IrArrayToSet ir, A a);

    public B visit(IrSetElement ir, A a);

    public B visit(IrJoinRelation ir, A a);

    public B visit(IrJoinFunction ir, A a);

    public B visit(IrSetDifference ir, A a);

    public B visit(IrSetIntersection ir, A a);

    public B visit(IrSetUnion ir, A a);

    public B visit(IrOffset ir, A a);

    public B visit(IrMask ir, A a);

    public B visit(IrSetTernary ir, A a);
}
