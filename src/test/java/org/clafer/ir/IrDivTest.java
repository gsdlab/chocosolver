package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import static org.junit.Assume.assumeFalse;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrDivTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar dividend, IrIntVar divisor, IrIntVar quotient) {
        assumeFalse(divisor.getDomain().contains(0));
        return equal(div(dividend, divisor), quotient);
    }

    @Solution
    public Constraint setup(IntVar dividend, IntVar divisor, IntVar quotient) {
        return dividend.getModel().div(dividend, divisor, quotient);
    }
}
