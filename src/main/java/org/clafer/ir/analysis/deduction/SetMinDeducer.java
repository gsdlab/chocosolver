package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSetMin;

/**
 *
 * @author jimmy
 */
class SetMinDeducer implements IntDeducer<IrSetMin> {

    @Override
    public void deduce(IrSetMin ir, Domain domain, Deduction deduction) {
        IrSetExpr set = ir.getSet();

        boolean nonEmpty = set.getCard().getLowBound() > 0
                || !domain.contains(ir.getDefaultValue());

        if (nonEmpty) {
            if (!set.getKer().intersects(domain)) {
                // Set needs another element.
                deduction.cardGreaterThanEqual(set, set.getKer().size() + 1);
                if (set.getKer().size() + 1 == set.getCard().getHighBound()) {
                    // Set can take only one more element.
                    deduction.envSubsetOf(set, set.getEnv().intersection(domain).union(set.getKer()));
                }
            }
            int min = domain.getLowBound();
            if (!set.getKer().isEmpty()) {
                min = Math.min(min, set.getKer().getLowBound());
            }
            if (set.getEnv().getLowBound() < min) {
                deduction.envRemove(set, Domains.boundDomain(set.getEnv().getLowBound(), min - 1));
            }
            if (domain.isConstant()) {
                deduction.kerContains(set, domain.getLowBound());
            }
        }
    }
}
