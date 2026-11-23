package org.clafer.ir;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.impl.FixedBoolVarImpl;
import org.chocosolver.solver.variables.impl.FixedIntVarImpl;
import org.chocosolver.solver.variables.view.IntView;
import org.clafer.ir.IrQuickTest.Check;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.compare;
import org.clafer.test.Term;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrArithmXYTest {

    @Check
    public void check(Solver solver) {
        assertTrue("Correct but not optimized.", solver.getModel().getNbCstrs() <= 1);
        assertTrue("Correct but not optimized.", solver.getModel().getNbVars() <= 4);
        for (Variable var : solver.getModel().getVars()) {
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
        return left.getModel().arithm(left, op.getSyntax(), right);
    }
}
