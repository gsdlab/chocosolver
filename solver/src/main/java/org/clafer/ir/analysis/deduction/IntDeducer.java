package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrIntExpr;

/**
 *
 * @author jimmy
 */
interface IntDeducer<I extends IrIntExpr> {

    void deduce(I ir, Domain domain, Deduction deduction);
}
