package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrBoolExprVisitor<A, B> {

    public B visit(IrBoolLiteral ir, A a);

    public B visit(IrNot ir, A a);

    public B visit(IrAnd ir, A a);

    public B visit(IrOr ir, A a);

    public B visit(IrImplies ir, A a);

    public B visit(IrNotImplies ir, A a);

    public B visit(IrIfOnlyIf ir, A a);

    public B visit(IrBetween ir, A a);

    public B visit(IrNotBetween ir, A a);

    public B visit(IrCompare ir, A a);

    public B visit(IrSetEquality ir, A a);

    public B visit(IrMember ir, A a);

    public B visit(IrNotMember ir, A a);
}
