package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrNotMember;
import org.clafer.ir.IrSetExpr;

/**
 *
 * @author jimmy
 */
class NotMemberDeducer implements BoolDeducer<IrNotMember> {

    @Override
    public void deduce(IrNotMember ir, Deduction deduction) {
        IrIntExpr element = ir.getElement();
        IrSetExpr set = ir.getSet();
        deduction.notWithin(element, set.getKer());
        if (element.getDomain().isConstant()) {
            deduction.envRemove(set, element.getDomain());
        }
    }
}
