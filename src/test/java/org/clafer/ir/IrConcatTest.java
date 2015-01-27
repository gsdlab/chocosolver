package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CStringVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrConcatTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrStringVar left, IrStringVar right, IrStringVar concat) {
        return equal(concat, concat(left, right));
    }

    @Solution
    public Constraint setup(CStringVar left, CStringVar right, CStringVar concat) {
        return Constraints.concat(
                left.getChars(), left.getLength(),
                right.getChars(), right.getLength(),
                concat.getChars(), concat.getLength());
    }
}
