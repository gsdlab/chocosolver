package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CStringVar;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.equal;
import static org.clafer.ir.Irs.length;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrLengthTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar length, IrStringVar word) {
        return equal(length, length(word));
    }

    @Solution
    public Constraint setup(IntVar length, CStringVar word) {
        return length.getModel().arithm(length, "=", word.getLength());
    }
}
