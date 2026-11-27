package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domain;
import org.clafer.ir.IrSetExpr;

/**
 *
 * @author jimmy
 */
interface SetDeducer<I extends IrSetExpr> {

    void deduceKer(I ir, Domain ker, Deduction deduction);

    void deduceEnv(I ir, Domain env, Deduction deduction);

    void deduceCard(I ir, Domain card, Deduction deduction);
}
