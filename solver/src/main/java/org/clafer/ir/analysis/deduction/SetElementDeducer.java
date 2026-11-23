package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSetArrayExpr;
import org.clafer.ir.IrSetElement;

/**
 *
 * @author jimmy
 */
class SetElementDeducer implements SetDeducer<IrSetElement> {

    @Override
    public void deduceKer(IrSetElement ir, Domain ker, Deduction deduction) {
        IrIntExpr index = ir.getIndex();
        IrSetArrayExpr array = ir.getArray();
        Domain dom = index.getDomain().retainAll(i -> ker.isSubsetOf(array.getEnvs()[i]));
        deduction.within(index, dom);
    }

    @Override
    public void deduceEnv(IrSetElement ir, Domain env, Deduction deduction) {
        IrIntExpr index = ir.getIndex();
        IrSetArrayExpr array = ir.getArray();
        Domain dom = index.getDomain().retainAll(i -> env.isSupersetOf(array.getKers()[i]));
        deduction.within(index, dom);
    }

    @Override
    public void deduceCard(IrSetElement ir, Domain card, Deduction deduction) {
        IrIntExpr index = ir.getIndex();
        IrSetArrayExpr array = ir.getArray();
        Domain dom = index.getDomain().retainAll(i -> card.intersects(array.getCards()[i]));
        deduction.within(index, dom);
    }
}
