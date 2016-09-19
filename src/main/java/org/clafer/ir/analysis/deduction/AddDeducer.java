package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrAdd;
import org.clafer.ir.IrIntExpr;

/**
 *
 * @author jimmy
 */
class AddDeducer implements IntDeducer<IrAdd> {

    @Override
    public void deduce(IrAdd ir, Domain domain, Deduction deduction) {
        IrIntExpr[] addends = ir.getAddends();
        if (addends.length == 1) {
            deduction.within(addends[0], domain.offset(-ir.getOffset()));
        } else {
            for (IrIntExpr addend : addends) {
                Domain addendDomain = addend.getDomain();
                Domain bound = addendDomain.boundBetween(
                        domain.getLowBound() - ir.getDomain().getHighBound() + addendDomain.getHighBound(),
                        domain.getHighBound() - ir.getDomain().getLowBound() + addendDomain.getLowBound());
                deduction.within(addend, bound);
            }
        }
    }
}
