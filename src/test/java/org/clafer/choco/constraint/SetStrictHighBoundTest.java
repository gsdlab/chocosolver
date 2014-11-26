package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.CSetVar;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetStrictHighBoundTest {

    @ConstraintQuickTest.Input(solutions = 129)
    public Object testSumSet(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     set <- powerset [-4..3]
         *     guard $ length set <= 2
         *     bound <- [-4..4]
         *     guard $ all (< bound) set
         *     return (set, bound)
         */
        return $(cset("set", env(-4, -3, -2, -1, 0, 1, 2, 3), ker(), card(0, 1, 2), solver),
                enumerated("bound", -4, 4, solver));
    }

    @ConstraintQuickTest.Check
    public void check(int[] set, int bound) {
        if (set.length > 0) {
            assertTrue(set[set.length - 1] < bound);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(CSetVar set, IntVar bound) {
        return Constraints.stritctHighBound(set.getSet(), bound);
    }
}
