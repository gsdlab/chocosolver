package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrCompare;
import org.clafer.ir.IrIntExpr;

/**
 *
 * @author jimmy
 */
class CompareDeducer implements BoolDeducer<IrCompare> {

    @Override
    public void deduce(IrCompare ir, Deduction deduction) {
        IrIntExpr left = ir.getLeft();
        IrIntExpr right = ir.getRight();
        switch (ir.getOp()) {
            case Equal:
                deduction.equal(left, right);
                break;
            case NotEqual:
                deduction.notEqual(left, right);
                break;
            case LessThan:
                deduction.lessThan(left, right);
                break;
            case LessThanEqual:
                deduction.lessThanEqual(left, right);
                break;
        }
    }
}
