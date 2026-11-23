package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrBoolVar;

/**
 *
 * @author jimmy
 */
class BoolVarDeducer implements BoolDeducer<IrBoolVar> {

    @Override
    public void deduce(IrBoolVar ir, Deduction deduction) {
        deduction.equal(ir, 1);
    }
}
