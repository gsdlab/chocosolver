package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Check;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.Term;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.variables.IntVar;
import solver.variables.Variable;
import solver.variables.impl.FixedBoolVarImpl;
import solver.variables.impl.FixedIntVarImpl;
import solver.variables.view.IntView;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrArithmXYTest {

    @Check
    public void check(Solver solver) {
        assertTrue("Correct but not optimized.", solver.getNbCstrs() <= 1);
        assertTrue("Correct but not optimized.", solver.getNbVars() <= 4);
        for (Variable var : solver.getVars()) {
            assertFalse("Correct but not optimized.",
                    var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl));
            assertFalse("Correct but not optimized.", var instanceof IntView);
        }
    }

    @Test(timeout = 60000)
    public IrBoolExpr setup(Term left, IrCompare.Op op, Term right) {
        return compare(left.toIrExpr(), op, right.toIrExpr());
    }

    @Solution
    public Constraint setup(IntVar left, IrCompare.Op op, IntVar right) {
        return ICF.arithm(left, op.getSyntax(), right);
    }
}
