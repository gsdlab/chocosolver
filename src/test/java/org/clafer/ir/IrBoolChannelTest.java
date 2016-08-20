package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.boolChannel;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrBoolChannelTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrBoolVar[] bools, IrSetVar set) {
        return boolChannel(bools, set);
    }

    @Solution
    public Constraint setup(BoolVar[] bools, SetVar set) {
        return set.getModel().setBoolsChanneling(bools, set, 0);
    }
}
