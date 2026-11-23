package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrIfOnlyIf;

/**
 *
 * @author jimmy
 */
class IfOnlyIfDeducer implements BoolDeducer<IrIfOnlyIf> {

    @Override
    public void deduce(IrIfOnlyIf ir, Deduction deduction) {
        deduction.equal(ir.getLeft(), ir.getRight());
    }
}
