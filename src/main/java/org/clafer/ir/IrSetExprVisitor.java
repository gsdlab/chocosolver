package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrSetExprVisitor<A, B> {

    public B visit(IrSetVar ir, A a);

    public B visit(IrSingleton ir, A a);

    public B visit(IrArrayToSet ir, A a);

    public B visit(IrJoin ir, A a);

    public B visit(IrJoinRef ir, A a);

    public B visit(IrUnion ir, A a);
}
