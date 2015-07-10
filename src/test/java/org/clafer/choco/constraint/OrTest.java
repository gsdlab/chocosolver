package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class OrTest {

    @Input(solutions = 31)
    public static Object testOr(Solver solver) {
        return $(boolArray("bool", 5, solver));
    }

    @Input(solutions = 1)
    public static Object testOneVar(Solver solver) {
        return $(boolArray("bool", 1, solver));
    }

    @Input(solutions = 2)
    public static Object testTautology(Solver solver) {
        return $(new BoolVar[]{solver.ONE(), solver.ZERO(), bool("bool", solver)});
    }

    @Input(solutions = 0)
    public static Object testFalseTautology(Solver solver) {
        return $(new BoolVar[]{solver.ZERO(), solver.ZERO()});
    }

    @Check
    public static void check(int[] bools) {
        assertTrue(Util.sum(bools) >= 1);
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty BoolVar[] bools) {
        return Constraints.or(bools);
    }
}
