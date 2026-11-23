package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrSetDifference;

/**
 *
 * @author jimmy
 */
class SetDifferenceDeducer implements SetDeducer<IrSetDifference> {

    @Override
    public void deduceKer(IrSetDifference ir, Domain ker, Deduction deduction) {
        deduction.kerContains(ir.getMinuend(), ker);
        deduction.envRemove(ir.getSubtrahend(), ker);
    }

    @Override
    public void deduceEnv(IrSetDifference ir, Domain env, Deduction deduction) {
        deduction.kerContains(ir.getSubtrahend(), ir.getMinuend().getKer().difference(env));
    }

    @Override
    public void deduceCard(IrSetDifference ir, Domain card, Deduction deduction) {
        deduction.cardGreaterThanEqual(ir.getMinuend(), card.getLowBound());
    }
}
