package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrMinus;

/**
 *
 * @author jimmy
 */
class MinusDeducer implements IntDeducer<IrMinus> {

    @Override
    public void deduce(IrMinus ir, Domain domain, Deduction deduction) {
        deduction.within(ir.getExpr(), domain.minus());
    }
}
