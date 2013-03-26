package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrConstraint {

    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a);
}
