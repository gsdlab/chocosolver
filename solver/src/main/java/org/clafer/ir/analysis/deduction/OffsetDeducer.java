package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrOffset;

/**
 *
 * @author jimmy
 */
class OffsetDeducer implements SetDeducer<IrOffset> {

    @Override
    public void deduceKer(IrOffset ir, Domain ker, Deduction deduction) {
        deduction.kerContains(ir.getSet(), ker.offset(-ir.getOffset()));
    }

    @Override
    public void deduceEnv(IrOffset ir, Domain env, Deduction deduction) {
        deduction.envSubsetOf(ir.getSet(), env.offset(-ir.getOffset()));
    }

    @Override
    public void deduceCard(IrOffset ir, Domain card, Deduction deduction) {
        deduction.cardWithin(ir.getSet(), card);
    }
}
