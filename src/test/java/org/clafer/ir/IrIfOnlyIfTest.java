package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.ifOnlyIf;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrIfOnlyIfTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrBoolVar left, IrBoolVar right) {
        return ifOnlyIf(left, right);
    }

    @Solution
    public Constraint setup(BoolVar left, BoolVar right) {
        return left.getModel().arithm(left, "=", right);
    }
}
