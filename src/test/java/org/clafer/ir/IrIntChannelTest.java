package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.NonEmpty;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrIntChannelTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(@NonEmpty IrIntVar[] ints, @NonEmpty IrSetVar[] sets) {
        return intChannel(ints, sets);
    }

    @Solution
    public Constraint setup(IntVar[] ints, SetVar[] sets) {
        return Constraints.intChannel(sets, ints);
    }
}
