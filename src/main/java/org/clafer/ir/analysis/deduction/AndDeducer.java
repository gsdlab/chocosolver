package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrAnd;
import org.clafer.ir.IrBoolExpr;

/**
 *
 * @author jimmy
 */
class AndDeducer implements BoolDeducer<IrAnd> {

    @Override
    public void deduce(IrAnd ir, Deduction deduction) {
        for (IrBoolExpr operand : ir.getOperands()) {
            deduction.tautology(operand);
        }
    }
}
