package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import static org.clafer.domain.Domains.boundDomain;
import org.clafer.ir.IrIntChannel;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSetExpr;

/**
 *
 * @author jimmy
 */
class IntChannelDeducer implements BoolDeducer<IrIntChannel> {

    @Override
    public void deduce(IrIntChannel ir, Deduction deduction) {
        IrIntExpr[] ints = ir.getInts();
        IrSetExpr[] sets = ir.getSets();

        Domain kers = Domains.EmptyDomain;

        for (int i = 0; i < ints.length; i++) {
            int ii = i;
            Domain domain = ints[i].getDomain().retainAll(
                    j -> j >= 0 && j < sets.length && sets[j].getEnv().contains(ii));
            deduction.within(ints[i], domain);
        }
        int lowCards = 0;
        int highCards = 0;
        for (IrSetExpr set : sets) {
            kers = kers.union(set.getKer());
            lowCards += set.getCard().getLowBound();
            highCards += set.getCard().getHighBound();
        }
        for (int i = 0; i < sets.length; i++) {
            int ii = i;
            Domain env = sets[i].getEnv().retainAll(
                    j -> j >= 0 && j < ints.length && ints[j].getDomain().contains(ii));
            Domain ker = env.retainAll(j -> ints[j].getDomain().isConstant());
            deduction.kerContains(sets[i], ker);
            env = env.difference(kers);
            env = env.union(sets[i].getKer());
            deduction.envSubsetOf(sets[i], env);
            Domain card = boundDomain(
                    ints.length - highCards + sets[i].getCard().getHighBound(),
                    ints.length - lowCards + sets[i].getCard().getLowBound());
            deduction.cardWithin(sets[i], card);
        }
    }
}
