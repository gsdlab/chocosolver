package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrSelectN;
import org.clafer.ir.IrUtil;

/**
 *
 * @author jimmy
 */
class SelectNDeducer implements BoolDeducer<IrSelectN> {

    @Override
    public void deduce(IrSelectN ir, Deduction deduction) {
        IrBoolExpr[] bools = ir.getBools();
        IrIntExpr n = ir.getN();

        deduction.lessThanEqual(n, bools.length);
        for (int i = 0; i < bools.length; i++) {
            if (bools[i].getDomain().isTrue() && i >= n.getDomain().getLowBound()) {
                deduction.greaterThanEqual(n, i + 1);
            } else if (bools[i].getDomain().isFalse() && i < n.getDomain().getHighBound()) {
                deduction.lessThanEqual(n, i);
            }
        }
        for (int i = 0; i < n.getDomain().getLowBound() && i < bools.length; i++) {
            deduction.equal(bools[i], 1);
        }
        for (int i = n.getDomain().getHighBound(); i >= 0 && i < bools.length; i++) {
            deduction.equal(bools[i], 0);
        }
    }
}
