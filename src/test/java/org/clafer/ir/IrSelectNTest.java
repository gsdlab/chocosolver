package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.selectN;
import org.junit.Test;
import org.junit.runner.RunWith;

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
