package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSortStrings;

/**
 *
 * @author jimmy
 */
class SortStringsDeducer implements BoolDeducer<IrSortStrings> {

    @Override
    public void deduce(IrSortStrings ir, Deduction deduction) {
        IrIntExpr[][] strings = ir.getStrings();
        for (int i = 0; i < strings.length - 1; i++) {
            if (ir.isStrict()) {
                deduction.lexThan(strings[i], strings[i + 1]);
            } else {
                deduction.lexThanEqual(strings[i], strings[i + 1]);
            }
        }
    }
}
