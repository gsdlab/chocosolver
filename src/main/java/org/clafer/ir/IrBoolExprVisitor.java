package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrBoolExprVisitor<A, B> {

    public B visit(IrBoolVar ir, A a);

    public B visit(IrNot ir, A a);

    public B visit(IrAnd ir, A a);

    public B visit(IrImplies ir, A a);

    public B visit(IrCompare ir, A a);

    public B visit(IrSetEquality ir, A a);
}
