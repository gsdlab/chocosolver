package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import static org.clafer.domain.Domains.constantDomain;
import org.clafer.ir.IrCount;
import org.clafer.ir.IrIntExpr;

/**
 *
 * @author jimmy
 */
class CountDeducer implements IntDeducer<IrCount> {

    @Override
    public void deduce(IrCount ir, Domain domain, Deduction deduction) {
        int value = ir.getValue();
        int possibles = 0;
        int mandatories = 0;
        for (IrIntExpr element : ir.getArray()) {
            if (element.getDomain().contains(value)) {
                if (element.getDomain().isConstant()) {
                    mandatories++;
                } else {
                    possibles++;
                }
            }
        }
        if (possibles + mandatories <= domain.getLowBound()) {
            for (IrIntExpr element : ir.getArray()) {
                if (element.getDomain().contains(value)) {
                    deduction.equal(element, value);
                }
            }
        } else if (mandatories >= domain.getHighBound()) {
            for (IrIntExpr element : ir.getArray()) {
                if (element.getDomain().contains(value)) {
                    deduction.notEqual(element, element);
                }
            }
        }
    }
}
