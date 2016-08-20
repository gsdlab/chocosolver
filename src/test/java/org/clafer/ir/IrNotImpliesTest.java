package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.notImplies;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrNotImpliesTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrBoolVar antecedent, IrBoolVar consequent) {
        return notImplies(antecedent, consequent);
    }

    @Solution
    public Constraint setup(BoolVar antecedent, BoolVar consequent) {
        return antecedent.getModel().arithm(antecedent, ">", consequent);
    }
}
