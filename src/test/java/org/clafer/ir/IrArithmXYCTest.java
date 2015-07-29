package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Check;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.Term;
import static org.junit.Assert.*;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.impl.FixedBoolVarImpl;
import org.chocosolver.solver.variables.impl.FixedIntVarImpl;
import org.chocosolver.solver.variables.view.IntView;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrArithmXYCTest {

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
    public IrBoolExpr setup(boolean left, IrCompare.Op op, Term var1, Term var2, Term var3) {
        assumeTrue(
                var1.getIrVar() instanceof IrConstant
                || var2.getIrVar() instanceof IrConstant
                || var3.getIrVar() instanceof IrConstant);
        IrIntExpr add = add(var1.toIrExpr(), var2.toIrExpr());
        return left
                ? compare(add, op, var3.toIrExpr())
                : compare(var3.toIrExpr(), op, add);
    }

    @Solution
    public Constraint setup(boolean left, IrCompare.Op op, IntVar var1, IntVar var2, IntVar var3) {
        return ICF.sum(new IntVar[]{var1, var2}, left ? op.getSyntax() : reverse(op), var3);
    }

    private String reverse(IrCompare.Op op) {
        switch (op) {
            case Equal:
            case NotEqual:
                return op.getSyntax();
            case LessThan:
                return ">";
            case LessThanEqual:
                return ">=";
            default:
                throw new IllegalArgumentException();
        }
    }
}
