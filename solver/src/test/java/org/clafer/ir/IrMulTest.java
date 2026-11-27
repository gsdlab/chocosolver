package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.domain.Domains;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.equal;
import static org.clafer.ir.Irs.mul;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrMulTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar multiplicand, IrIntVar multiplier, IrIntVar product) {
        return equal(mul(multiplicand, multiplier, Domains.boundDomain(-1000, 1000)), product);
    }

    @Solution
    public Constraint setup(IntVar multiplicand, IntVar multiplier, IntVar product) {
        return multiplicand.getModel().times(multiplicand, multiplier, product);
    }
}
