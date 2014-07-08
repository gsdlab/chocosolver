package org.clafer.ir;

/**
 * @param <A> the parameter type
 * @param <B> the return type
 * @author jimmy
 */
public abstract class IrBoolExprVisitorAdapter<A, B> implements IrBoolExprVisitor<A, B> {

    @Override
    public B visit(IrBoolVar ir, A a) {
        return null;
    }

    @Override
    public B visit(IrNot ir, A a) {
        return null;
    }

    @Override
    public B visit(IrAnd ir, A a) {
        return null;
    }

    @Override
    public B visit(IrLone ir, A a) {
        return null;
    }

    @Override
    public B visit(IrOne ir, A a) {
        return null;
    }

    @Override
    public B visit(IrOr ir, A a) {
        return null;
    }

    @Override
    public B visit(IrImplies ir, A a) {
        return null;
    }

    @Override
    public B visit(IrNotImplies ir, A a) {
        return null;
    }

    @Override
    public B visit(IrIfThenElse ir, A a) {
        return null;
    }

    @Override
    public B visit(IrIfOnlyIf ir, A a) {
        return null;
    }

    @Override
    public B visit(IrXor ir, A a) {
        return null;
    }

    @Override
    public B visit(IrWithin ir, A a) {
        return null;
    }

    @Override
    public B visit(IrCompare ir, A a) {
        return null;
    }

    @Override
    public B visit(IrSetEquality ir, A a) {
        return null;
    }

    @Override
    public B visit(IrStringCompare ir, A a) {
        return null;
    }

    @Override
    public B visit(IrMember ir, A a) {
        return null;
    }

    @Override
    public B visit(IrNotMember ir, A a) {
        return null;
    }

    @Override
    public B visit(IrSubsetEq ir, A a) {
        return null;
    }

    @Override
    public B visit(IrBoolChannel ir, A a) {
        return null;
    }

    @Override
    public B visit(IrIntChannel ir, A a) {
        return null;
    }

    @Override
    public B visit(IrSortStrings ir, A a) {
        return null;
    }

    @Override
    public B visit(IrSortSets ir, A a) {
        return null;
    }

    @Override
    public B visit(IrSortStringsChannel ir, A a) {
        return null;
    }

    @Override
    public B visit(IrAllDifferent ir, A a) {
        return null;
    }

    @Override
    public B visit(IrSelectN ir, A a) {
        return null;
    }

    @Override
    public B visit(IrAcyclic ir, A a) {
        return null;
    }

    @Override
    public B visit(IrUnreachable ir, A a) {
        return null;
    }

    @Override
    public B visit(IrFilterString ir, A a) {
        return null;
    }

    @Override
    public B visit(IrPrefix ir, A a) {
        return null;
    }

    @Override
    public B visit(IrSuffix ir, A a) {
        return null;
    }
}
