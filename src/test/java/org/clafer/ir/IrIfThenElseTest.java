package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.ifThenElse;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrIfThenElseTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrBoolVar antecedent, IrBoolVar consequent, IrBoolVar alternative) {
        return ifThenElse(antecedent, consequent, alternative);
    }

    @Solution
    public Constraint setup(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        return Constraints.ifThenElse(antecedent, consequent, alternative);
    }
}
