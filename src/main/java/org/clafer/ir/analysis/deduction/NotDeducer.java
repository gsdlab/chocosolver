package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrNot;

/**
 *
 * @author jimmy
 */
class NotDeducer implements BoolDeducer<IrNot> {

    @Override
    public void deduce(IrNot ir, Deduction deduction) {
        deduction.equal(ir.getExpr(), 0);
    }
}
