package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
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

        if (nonEmpty && !set.getKer().intersects(domain)) {
            deduction.cardGreaterThanEqual(set, set.getKer().size() + 1);
            if (set.getKer().size() + 1 == set.getCard().getHighBound()) {
                deduction.envSubsetOf(set, set.getEnv().intersection(domain).union(set.getKer()));
            }
        }
    }
}
