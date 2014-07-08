package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class LoneTest {

    @Input(solutions = 6)
    public Object testLone(Solver solver) {
        return $(boolArray("var", 5, solver));
    }

    @Input(solutions = 2)
    public Object testOneVar(Solver solver) {
        return $(boolArray("bool", 1, solver));
    }

    @Input(solutions = 2)
    public Object testTautology(Solver solver) {
        return $(new BoolVar[]{solver.ZERO, bool("bool", solver)});
    }

    @Input(solutions = 0)
    public Object testFalseTautology(Solver solver) {
        return $(new BoolVar[]{solver.ONE, bool("bool", solver), solver.ONE});
    }

    @Check
    public void check(int[] bools) {
        assertTrue(Util.sum(bools) <= 1);
    }

    @Test(timeout = 60000)
    public Constraint quickTest(@NonEmpty BoolVar[] bools) {
        return Constraints.lone(bools);
    }
}
