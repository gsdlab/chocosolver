package org.clafer.ir;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.add;
import static org.clafer.ir.Irs.equal;
import org.junit.Test;
import org.junit.runner.RunWith;

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
        Model model = sum.getModel();
        return is.length > 0 ? model.sum(is, "=", sum) : model.arithm(sum, "=", 0);
    }
}
