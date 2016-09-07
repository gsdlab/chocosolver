package org.clafer.ir.analysis.deduction;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.util.tools.MathUtils;
import org.clafer.domain.Domain;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrMul;
import org.clafer.ir.IrUtil;

/**
 *
 * @author jimmy
 */
class MulDeducer implements IntDeducer<IrMul> {

    @Override
    public void deduce(IrMul ir, Domain domain, Deduction deduction) {
        // TODO improve
        Integer multiplicand = IrUtil.getConstant(ir.getMultiplicand());
        if (multiplicand != null) {
            propagateMul(domain, multiplicand, ir.getMultiplier(), deduction);
        }
        Integer multiplier = IrUtil.getConstant(ir.getMultiplier());
        if (multiplier != null) {
            propagateMul(domain, multiplier, ir.getMultiplicand(), deduction);
        }
    }

    private void propagateMul(Domain product, int scale, IrIntExpr multiplicand, Deduction deduction) {
        if (scale > 0) {
            deduction.greaterThanEqual(multiplicand, MathUtils.divCeil(product.getLowBound(), scale));
            deduction.lessThanEqual(multiplicand, MathUtils.divFloor(product.getHighBound(), scale));
        } else if (scale < 0) {
            deduction.lessThanEqual(multiplicand, MathUtils.divFloor(product.getLowBound(), scale));
//            deduction.greaterThanEqual(multiplicand, MathUtils.divFloor(product.getHighBound(), scale));
        }
    }
}
