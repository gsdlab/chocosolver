package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrTernary;

/**
 *
 * @author jimmy
 */
class TernaryDeducer implements IntDeducer<IrTernary> {

    @Override
    public void deduce(IrTernary ir, Domain domain, Deduction deduction) {
        if (!domain.intersects(ir.getConsequent().getDomain())) {
            deduction.contradiction(ir.getAntecedent());
        } else if (!domain.intersects(ir.getAlternative().getDomain())) {
            deduction.tautology(ir.getAntecedent());
        }
    }
}
