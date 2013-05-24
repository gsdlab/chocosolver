package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrSetExprVisitor<A, B> {

    public B visit(IrSetLiteral ir, A a);

    public B visit(IrSingleton ir, A a);

    public B visit(IrArrayToSet ir, A a);

    public B visit(IrJoin ir, A a);

    public B visit(IrJoinRef ir, A a);

    public B visit(IrSetDifference ir, A a);

    public B visit(IrSetIntersection ir, A a);

    public B visit(IrSetUnion ir, A a);

    public B visit(IrOffset ir, A a);

    public B visit(IrSetTernary ir, A a);
}
