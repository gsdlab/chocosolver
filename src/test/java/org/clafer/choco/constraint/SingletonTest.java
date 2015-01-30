package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
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
public class SingletonTest {

    @Input(solutions = 5)
    public Object testSingleton(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     i <- [-3..2]
         *     s <- powerset [-2..3]
         *     guard $ [i] == s
         *     return (i, s)
         */
        return $(enumerated("i", -3, 2, solver),
                set("s", -2, 3, solver));
    }

    @Check
    public void check(int i, int[] s) {
        assertArrayEquals(new int[]{i}, s);
    }

    @Test(timeout = 60000)
    public Constraint setup(IntVar i, SetVar s) {
        return Constraints.singleton(i, s);
    }
}
