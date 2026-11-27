package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrAllDifferent;

/**
 *
 * @author jimmy
 */
class AllDifferentDeducer implements BoolDeducer<IrAllDifferent> {

    @Override
    public void deduce(IrAllDifferent ir, Deduction deduction) {
        deduction.allDifferent(ir.getOperands());
    }
}
