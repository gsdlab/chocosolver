package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.member;
import org.junit.Test;
import org.junit.runner.RunWith;

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
        return Constraints.member(element, set);
    }
}
