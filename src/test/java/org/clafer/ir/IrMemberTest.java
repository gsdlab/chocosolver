package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrMemberTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar element, IrSetVar set) {
        return member(element, set);
    }

    @Solution
    public Constraint setup(IntVar element, SetVar set) {
        return SCF.member(element, set);
    }
}
