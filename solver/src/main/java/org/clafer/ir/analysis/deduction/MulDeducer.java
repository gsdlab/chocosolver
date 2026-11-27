package org.clafer.ir.analysis.deduction;

import org.chocosolver.util.tools.MathUtils;
import org.clafer.domain.Domain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrMul;

/**
 *
 * @author jimmy
 */
class MulDeducer implements IntDeducer<IrMul> {

    @Override
    public void deduce(IrMul ir, Domain domain, Deduction deduction) {
        // TODO improve
        IrIntExpr multiplicand = ir.getMultiplicand();
        IrIntExpr multiplier = ir.getMultiplier();
        if (multiplicand.isConstant()) {
            propagateMul(domain, multiplicand.getLowBound(), ir.getMultiplier(), deduction);
        }
        if (multiplier.isConstant()) {
            propagateMul(domain, multiplier.getLowBound(), ir.getMultiplicand(), deduction);
        }
    }

    private void propagateMul(Domain product, int scale, IrIntExpr multiplicand, Deduction deduction) {
        if (scale > 0) {
            deduction.greaterThanEqual(multiplicand, MathUtils.divCeil(product.getLowBound(), scale));
            deduction.lessThanEqual(multiplicand, MathUtils.divFloor(product.getHighBound(), scale));
        } else if (scale < 0) {
            deduction.lessThanEqual(multiplicand, MathUtils.divFloor(product.getLowBound(), scale));
            deduction.greaterThanEqual(multiplicand, MathUtils.divCeil(product.getHighBound(), scale));
        }
    }
}
