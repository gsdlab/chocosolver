package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import static org.junit.Assume.assumeFalse;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrModTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar dividend, IrIntVar divisor, IrIntVar remainder) {
        assumeFalse(divisor.getDomain().contains(0));
        return equal(mod(dividend, divisor), remainder);
    }

    @Solution
    public Constraint setup(IntVar dividend, IntVar divisor, IntVar remainder) {
        return ICF.mod(dividend, divisor, remainder);
    }
}
