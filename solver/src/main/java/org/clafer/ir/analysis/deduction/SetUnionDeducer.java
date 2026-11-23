package org.clafer.ir.analysis.deduction;

import java.util.PrimitiveIterator;
import org.clafer.common.Util;
import org.clafer.domain.Domain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetUnion;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
class SetUnionDeducer implements SetDeducer<IrSetUnion> {

    @Override
    public void deduceKer(IrSetUnion ir, Domain ker, Deduction deduction) {
        PrimitiveIterator.OfInt iter = ker.difference(ir.getKer()).iterator();
        while (iter.hasNext()) {
            int val = iter.next();
            Util.<IrSetExpr>findUnique(ir.getOperands(), x -> x.getEnv().contains(val))
                    .ifPresent(index
                            -> deduction.kerContains(index, val));
        }
    }

    @Override
    public void deduceEnv(IrSetUnion ir, Domain env, Deduction deduction) {
        for (IrSetExpr operand : ir.getOperands()) {
            deduction.envSubsetOf(operand, env);
        }
    }

    @Override
    public void deduceCard(IrSetUnion ir, Domain card, Deduction deduction) {
        IrSetExpr[] operands = ir.getOperands();

        if (ir.isDisjoint()) {
            IrIntExpr[] cards = new IrIntExpr[operands.length];
            for (int i = 0; i < cards.length; i++) {
                cards[i] = Irs.card(operands[i]);
            }
            deduction.within(Irs.add(cards), card);
        } else {
            for (IrSetExpr operand : operands) {
                deduction.cardLessThanEqual(operand, card.getHighBound());
            }
        }
    }
}
