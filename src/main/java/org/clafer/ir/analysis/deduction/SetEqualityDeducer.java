package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrSetEquality;

/**
 *
 * @author jimmy
 */
class SetEqualityDeducer implements BoolDeducer<IrSetEquality> {

    @Override
    public void deduce(IrSetEquality ir, Deduction deduction) {
        switch (ir.getOp()) {
            case Equal:
                deduction.equal(ir.getLeft(), ir.getRight());
                break;
            case NotEqual:
        }
    }
}
