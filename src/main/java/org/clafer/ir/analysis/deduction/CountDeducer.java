package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrCount;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrUtil;

/**
 *
 * @author jimmy
 */
class CountDeducer implements IntDeducer<IrCount> {

    @Override
    public void deduce(IrCount ir, Domain domain, Deduction deduction) {
        int value = ir.getValue();
        IrIntExpr[] array = IrUtil.asArray(ir.getArray());
        int possibles = 0;
        int mandatories = 0;
        for (IrIntExpr element : array) {
            if (element.getDomain().contains(value)) {
                if (element.getDomain().isConstant()) {
                    mandatories++;
                } else {
                    possibles++;
                }
            }
        }
        if (possibles + mandatories <= domain.getLowBound()) {
            for (IrIntExpr element : array) {
                if (element.getDomain().contains(value)) {
                    deduction.equal(element, value);
                }
            }
        } else if (mandatories >= domain.getHighBound()) {
            for (IrIntExpr element : array) {
                if (!element.getDomain().isConstant()) {
                    deduction.notEqual(element, element);
                }
            }
        }
    }
}
