package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.domain.Domains;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrMulTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar multiplicand, IrIntVar multiplier, IrIntVar product) {
        return equal(mul(multiplicand, multiplier, Domains.Unbounded), product);
    }

    @Solution
    public Constraint setup(IntVar multiplicand, IntVar multiplier, IntVar product) {
        return multiplicand.getModel().times(multiplicand, multiplier, product);
    }
}
