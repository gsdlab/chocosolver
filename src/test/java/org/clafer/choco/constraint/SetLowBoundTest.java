package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetLowBoundTest {

    @Input(solutions = 511)
    public Object testSumSet(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     set <- powerset [-4..3]
         *     bound <- [-4..4]
         *     guard $ all (>= bound) set
         *     return (set, bound)
         */
        return $(set("set", env(-4, -3, -2, -1, 0, 1, 2, 3), ker(), solver),
                enumerated("bound", -4, 4, solver));
    }

    @Check
    public void check(int[] set, int bound) {
        if (set.length > 0) {
            assertTrue(set[0] >= bound);
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(SetVar set, IntVar bound) {
        return Constraints.lowBound(set, bound);
    }
}
