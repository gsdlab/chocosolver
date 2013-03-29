package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrAllDifferent implements IrConstraint {

    private final IrIntExpr[] operands;

    IrAllDifferent(IrIntExpr[] operands) {
        this.operands = Check.noNulls(operands);
    }

    public IrIntExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
