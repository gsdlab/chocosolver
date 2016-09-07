package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrWithin;

/**
 *
 * @author jimmy
 */
class WithinDeducer implements BoolDeducer<IrWithin> {

    @Override
    public void deduce(IrWithin ir, Deduction deduction) {
        deduction.within(ir.getValue(), ir.getRange());
    }
}
