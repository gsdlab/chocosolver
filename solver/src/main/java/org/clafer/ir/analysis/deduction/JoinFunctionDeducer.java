package org.clafer.ir.analysis.deduction;

import java.util.PrimitiveIterator;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import org.clafer.ir.IrIntArrayExpr;
import org.clafer.ir.IrJoinFunction;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
class JoinFunctionDeducer implements SetDeducer<IrJoinFunction> {

    @Override
    public void deduceKer(IrJoinFunction ir, Domain ker, Deduction deduction) {
        IrSetExpr take = ir.getTake();
        IrIntArrayExpr refs = ir.getRefs();

        PrimitiveIterator.OfInt iter = ker.difference(ir.getKer()).iterator();
        while (iter.hasNext()) {
            int i = iter.next();
            Util.findUnique(
                    take.getEnv().iterator(),
                    j -> refs.getDomains()[j].contains(i))
                    .ifPresent(index -> {
                        deduction.kerContains(take, index);
                        deduction.equal(Irs.get(refs, index), i);
                    });
        }
    }

    @Override
    public void deduceEnv(IrJoinFunction ir, Domain env, Deduction deduction) {
        IrSetExpr take = ir.getTake();
        IrIntArrayExpr refs = ir.getRefs();

        Domain domain = take.getEnv().retainAll(
                i -> refs.getDomains()[i].intersects(env));
        deduction.envSubsetOf(ir.getTake(), domain);

        take.getKer().forEach(
                i -> deduction.within(Irs.get(refs, i), env));
    }

    @Override
    public void deduceCard(IrJoinFunction ir, Domain card, Deduction deduction) {
        deduction.cardGreaterThanEqual(ir.getTake(), card.getLowBound());
        if (ir.hasGlobalCardinality()) {
            deduction.cardLessThanEqual(ir.getTake(), card.getHighBound() * ir.getGlobalCardinality());
        }
    }
}
