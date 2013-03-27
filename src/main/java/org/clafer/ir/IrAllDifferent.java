package org.clafer.ir;

import solver.constraints.IntConstraintFactory;

/**
 *
 * @author jimmy
 */
public class IrAllDifferent implements IrConstraint {

    private final IrIntExpr[] operands;

    IrAllDifferent(IrIntExpr[] operands) {
        this.operands = operands;
    }

    public IrIntExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
