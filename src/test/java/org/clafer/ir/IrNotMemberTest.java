package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrNotMemberTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar element, IrSetVar set) {
        return notMember(element, set);
    }

    @Solution
    public Constraint setup(IntVar element, SetVar set) {
        return Constraints.notMember(element, set);
    }
}
