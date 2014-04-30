package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import static org.junit.Assume.assumeFalse;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrDivTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar quotient, IrIntVar dividend, IrIntVar divisor) {
        assumeFalse(divisor.getDomain().contains(0));
        return equal(quotient, div(dividend, divisor));
    }

    @Solution
    public Constraint setup(IntVar quotient, IntVar dividend, IntVar divisor) {
        return ICF.eucl_div(dividend, divisor, quotient);
    }
}
