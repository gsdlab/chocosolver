package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrSingleton;

/**
 *
 * @author jimmy
 */
class SingletonDeducer implements SetDeducer<IrSingleton> {

    @Override
    public void deduceKer(IrSingleton ir, Domain ker, Deduction deduction) {
        int size = ker.size();
        deduction.failIf(size > 1);
        if (size == 1) {
            deduction.equal(ir.getValue(), ker.getLowBound());
        }
    }

    @Override
    public void deduceEnv(IrSingleton ir, Domain env, Deduction deduction) {
        deduction.within(ir.getValue(), env);
    }

    @Override
    public void deduceCard(IrSingleton ir, Domain card, Deduction deduction) {
        deduction.failIf(!card.contains(1));
    }
}
