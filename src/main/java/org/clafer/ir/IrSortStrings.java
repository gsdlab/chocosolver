package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSortStrings implements IrConstraint {

    private final IrIntExpr[][] strings;

    IrSortStrings(IrIntExpr[][] strings) {
        this.strings = Check.noNulls(strings);
    }

    public IrIntExpr[][] getStrings() {
        return strings;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
