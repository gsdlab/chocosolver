package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrArrayEquality;
import org.clafer.ir.IrIntArrayExpr;
import org.clafer.ir.Irs;

/**
 *
 * @author jimmy
 */
class ArrayEqualityDeducer implements BoolDeducer<IrArrayEquality> {

    @Override
    public void deduce(IrArrayEquality ir, Deduction deduction) {
        switch (ir.getOp()) {
            case Equal:
                IrIntArrayExpr left = ir.getLeft();
                IrIntArrayExpr right = ir.getRight();
                assert left.length() == right.length();
                for (int i = 0; i < left.length(); i++) {
                    deduction.equal(Irs.get(left, i), Irs.get(right, i));
                }
                break;
            case NotEqual:
                break;
        }
    }
}
