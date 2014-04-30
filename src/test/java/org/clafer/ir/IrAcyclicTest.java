package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.NonEmpty;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrAcyclicTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(@NonEmpty IrIntVar[] edges) {
        return acyclic(edges);
    }

    @Solution
    public Constraint setup(IntVar[] edges) {
        return Constraints.acyclic(edges);
    }
}
