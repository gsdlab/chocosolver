package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.count;
import static org.clafer.ir.Irs.equal;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrCountTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(int value, IrIntVar[] array, IrIntVar count) {
        return equal(count(value, Irs.array(array)), count);
    }

    @Solution
    public Constraint setup(int value, IntVar[] array, IntVar count) {
        return count.getModel().count(value, array, count);
    }
}
