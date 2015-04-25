package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.variables.IntVar;

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
        return is.length > 0 ? ICF.sum(is, sum) : ICF.arithm(sum, "=", 0);
    }
}
