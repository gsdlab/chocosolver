package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrMember;
import org.clafer.ir.IrSetExpr;

/**
 *
 * @author jimmy
 */
class MemberDeducer implements BoolDeducer<IrMember> {

    @Override
    public void deduce(IrMember ir, Deduction deduction) {
        IrIntExpr element = ir.getElement();
        IrSetExpr set = ir.getSet();

        Domain intersection = element.getDomain().intersection(set.getEnv());
        if (intersection.isConstant()) {
            deduction.within(element, intersection);
            deduction.kerContains(set, intersection);
        } else {
            deduction.within(element, set.getEnv());

            if (!element.getDomain().intersects(set.getKer())) {
                deduction.cardGreaterThanEqual(set, set.getKer().size() + 1);
            }
        }
    }
}
