package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrSelectNTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrBoolVar[] bools, IrIntVar n) {
        return selectN(bools, n);
    }

    @Solution
    public Constraint setup(BoolVar[] bools, IntVar n) {
        return Constraints.selectN(bools, n);
    }
}
