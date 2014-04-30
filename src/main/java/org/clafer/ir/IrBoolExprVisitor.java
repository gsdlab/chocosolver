package org.clafer.ir;

/**
 * Dynamic dispatch for IR boolean expressions.
 *
 * @param <A> the parameter type
 * @param <B> the return type
 * @author jimmy
 */
public interface IrBoolExprVisitor<A, B> {

    public B visit(IrRegister ir, A a);

    public B visit(IrBoolVar ir, A a);

    public B visit(IrNot ir, A a);

    public B visit(IrAnd ir, A a);

    public B visit(IrLone ir, A a);

    public B visit(IrOne ir, A a);

    public B visit(IrOr ir, A a);

    public B visit(IrImplies ir, A a);

    public B visit(IrNotImplies ir, A a);

    public B visit(IrIfThenElse ir, A a);

    public B visit(IrIfOnlyIf ir, A a);

    public B visit(IrXor ir, A a);

    public B visit(IrWithin ir, A a);

    public B visit(IrCompare ir, A a);

    public B visit(IrSetEquality ir, A a);

    public B visit(IrStringCompare ir, A a);

    public B visit(IrMember ir, A a);

    public B visit(IrNotMember ir, A a);

    public B visit(IrSubsetEq ir, A a);

    public B visit(IrBoolChannel ir, A a);

    public B visit(IrIntChannel ir, A a);

    public B visit(IrSortStrings ir, A a);

    public B visit(IrSortSets ir, A a);

    public B visit(IrSortStringsChannel ir, A a);

    public B visit(IrAllDifferent ir, A a);

    public B visit(IrSelectN ir, A a);

    public B visit(IrAcyclic ir, A a);

    public B visit(IrUnreachable ir, A a);

    public B visit(IrFilterString ir, A a);

    public B visit(IrPrefix ir, A a);

    public B visit(IrSuffix ir, A a);
}
