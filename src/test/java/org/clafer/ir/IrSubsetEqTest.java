package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.constraints.set.SCF;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrSubsetEqTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrSetVar subset, IrSetVar superSet) {
        return subsetEq(subset, superSet);
    }

    @Solution
    public Constraint setup(SetVar subset, SetVar superSet) {
        return SCF.subsetEq(new SetVar[]{subset, superSet});
    }
}
