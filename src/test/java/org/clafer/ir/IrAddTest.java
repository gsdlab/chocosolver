package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrAddTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar[] is, IrIntVar sum) {
        return equal(add(is), sum);
    }

    @Solution
    public Constraint setup(IntVar[] is, IntVar sum) {
        return ICF.sum(is, sum);
    }
}
