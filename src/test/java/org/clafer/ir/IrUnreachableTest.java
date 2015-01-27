package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrUnreachableTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(@NonEmpty IrIntVar[] edges, @Positive int from, @Positive int to) {
        assumeTrue(from < edges.length);
        assumeTrue(to < edges.length);
        return unreachable(edges, from, to);
    }

    @Solution
    public Constraint setup(IntVar[] edges, int from, int to) {
        return Constraints.unreachable(edges, from, to);
    }
}
