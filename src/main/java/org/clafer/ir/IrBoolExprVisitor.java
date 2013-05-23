package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrBoolExprVisitor<A, B> {

    public B visit(IrBoolLiteral ir, A a);

    public B visit(IrNot ir, A a);

    public B visit(IrAnd ir, A a);

    public B visit(IrLone ir, A a);

    public B visit(IrOne ir, A a);

    public B visit(IrOr ir, A a);

    public B visit(IrImplies ir, A a);

    public B visit(IrNotImplies ir, A a);

    public B visit(IrIfThenElse ir, A a);

    public B visit(IrIfOnlyIf ir, A a);

    public B visit(IrBetween ir, A a);

    public B visit(IrNotBetween ir, A a);

    public B visit(IrCompare ir, A a);

    public B visit(IrSetTest ir, A a);

    public B visit(IrMember ir, A a);

    public B visit(IrNotMember ir, A a);

    public B visit(IrBoolCast ir, A a);

    public B visit(IrBoolChannel ir, A a);

    public B visit(IrIntChannel ir, A a);

    public B visit(IrSortInts ir, A a);

    public B visit(IrSortStrings ir, A a);

    public B visit(IrAllDifferent ir, A a);

    public B visit(IrSelectN ir, A a);
}
