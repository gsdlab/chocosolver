package org.clafer.ir.analysis.deduction;

import org.chocosolver.solver.Model;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import static org.clafer.domain.Domains.constantDomain;
import org.clafer.ir.IrArrayToSet;
import org.clafer.ir.IrIntExpr;

/**
 *
 * @author jimmy
 */
class ArrayToSetDeducer implements SetDeducer<IrArrayToSet> {

    @Override
    public void deduceKer(IrArrayToSet ir, Domain ker, Deduction deduction) {
        ker.forEach(val -> {
            Util.findUnique(ir.getArray(), x -> x.getDomain().contains(val))
                    .ifPresent(index
                            -> deduction.within(index, constantDomain(val)));
        });
    }

    @Override
    public void deduceEnv(IrArrayToSet ir, Domain env, Deduction deduction) {
        for (IrIntExpr child : ir.getArray()) {
            deduction.within(child, env);
        }
    }

    @Override
    public void deduceCard(IrArrayToSet ir, Domain card, Deduction deduction) {
        deduction.atLeastNValue(ir.getArray(), card.getLowBound());
        deduction.atMostNValue(ir.getArray(), card.getHighBound());
    }
}
