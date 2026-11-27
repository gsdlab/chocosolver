package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrBoolExpr;

/**
 *
 * @author jimmy
 */
interface BoolDeducer<I extends IrBoolExpr> {

    void deduce(I ir, Deduction deduction);
}
