package org.clafer.ir.analysis.deduction;

import org.clafer.ir.IrSetExpr;
import org.clafer.ir.IrSubsetEq;

/**
 *
 * @author jimmy
 */
class SubsetEqDeducer implements BoolDeducer<IrSubsetEq> {

    @Override
    public void deduce(IrSubsetEq ir, Deduction deduction) {
        IrSetExpr subset = ir.getSubset();
        IrSetExpr superset = ir.getSuperset();

        deduction.envSubsetOf(subset, superset.getEnv());
        deduction.kerContains(superset, subset.getKer());
        deduction.cardLessThanEqual(subset, superset.getCard().getHighBound());
        deduction.cardGreaterThanEqual(superset, subset.getCard().getLowBound());
    }
}
