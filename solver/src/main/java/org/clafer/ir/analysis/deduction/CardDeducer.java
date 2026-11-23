package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrCard;

/**
 *
 * @author jimmy
 */
class CardDeducer implements IntDeducer<IrCard> {

    @Override
    public void deduce(IrCard ir, Domain domain, Deduction deduction) {
        deduction.cardWithin(ir.getSet(), domain);
    }
}
