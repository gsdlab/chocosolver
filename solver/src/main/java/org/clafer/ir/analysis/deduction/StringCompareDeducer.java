package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrStringCompare;
import org.clafer.ir.IrStringVar;

/**
 *
 * @author jimmy
 */
class StringCompareDeducer implements BoolDeducer<IrStringCompare> {

    @Override
    public void deduce(IrStringCompare ir, Deduction deduction) {
        if (ir.getLeft() instanceof IrStringVar && ir.getRight() instanceof IrStringVar) {
            IrStringVar left = (IrStringVar) ir.getLeft();
            IrStringVar right = (IrStringVar) ir.getRight();
            switch (ir.getOp()) {
                case Equal:
                    deduction.lexEqual(left.getCharVars(), right.getCharVars());
                    deduction.equal(left.getLengthVar(), right.getLengthVar());
                    break;
                case LessThan:
//                    deduction.lexThan(left.getCharVars(), right.getCharVars());
                    break;
                case LessThanEqual:
//                    deduction.lexThanEqual(left.getCharVars(), right.getCharVars());
                    break;
            }
        }
    }
}
