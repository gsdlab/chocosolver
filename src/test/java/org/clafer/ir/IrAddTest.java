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
    public IrBoolExpr setup(IrIntVar sum, IrIntVar[] is) {
        return equal(sum, add(is));
    }

    @Solution
    public Constraint setup(IntVar sum, IntVar[] is) {
        return ICF.sum(is, sum);
    }
}
