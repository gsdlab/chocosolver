package org.clafer.ir;

/**
 * Visitor pattern for constraints.
 *
 * @param <A> the type of the parameter during visits
 * @param <B> the return type of visits
 * @author jimmy
 */
public interface IrConstraintVisitor<A, B> {

    public B visit(IrBoolConstraint ir, A a);

    public B visit(IrBoolChannel ir, A a);

    public B visit(IrIntChannel ir, A a);

    public B visit(IrSortInts ir, A a);

    public B visit(IrAllDifferent ir, A a);

    public B visit(IrSelectN ir, A a);
}
