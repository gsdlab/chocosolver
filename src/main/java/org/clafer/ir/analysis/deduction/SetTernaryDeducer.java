package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrSetTernary;

/**
 *
 * @author jimmy
 */
class SetTernaryDeducer implements SetDeducer<IrSetTernary> {

    @Override
    public void deduceKer(IrSetTernary ir, Domain ker, Deduction deduction) {
        if (!ker.isSubsetOf(ir.getConsequent().getEnv())) {
            deduction.contradiction(ir.getAntecedent());
        } else if (!ker.isSubsetOf(ir.getAlternative().getEnv())) {
            deduction.tautology(ir.getAntecedent());
        }
    }

    @Override
    public void deduceEnv(IrSetTernary ir, Domain env, Deduction deduction) {
        if (!env.isSupersetOf(ir.getConsequent().getKer())) {
            deduction.contradiction(ir.getAntecedent());
        } else if (!env.isSupersetOf(ir.getAlternative().getKer())) {
            deduction.tautology(ir.getAntecedent());
        }
    }

    @Override
    public void deduceCard(IrSetTernary ir, Domain card, Deduction deduction) {
        if (!card.intersects(ir.getConsequent().getCard())) {
            deduction.contradiction(ir.getAntecedent());
        } else if (!card.intersects(ir.getAlternative().getCard())) {
            deduction.tautology(ir.getAntecedent());
        }
    }
}
