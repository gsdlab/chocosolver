package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.test.TestUtil.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class FilterStringTest {

    @Input(solutions = 216)
    public Object testFilterString(Solver solver) {
        /*
         * import Control.Monad
         * 
         * powerset = filterM (const [True, False])
         * 
         * solutions = do
         *     set <- powerset [0..2]
         *     string <- sequence $ replicate 3 [0..2]
         *     let result = [string !! i | i <- set]
         * 
         *     return (set, string, result)
         */
        return $(cset("set", 0, 2, solver),
                0,
                enumeratedArray("string", 3, 0, 2, solver),
                enumeratedArray("result", 3, -1, 2, solver));
    }

    @Check
    public void check(int[] set, int offset, int[] string, int[] result) {
        int i = 0;
        for (; i < set.length; i++) {
            assertTrue(set[i] - offset >= 0);
            assertTrue(set[i] - offset < string.length);
            assertTrue(i >= 0);
            assertTrue(i < result.length);
            assertEquals(string[set[i] - offset], result[i]);
        }
        for (; i < result.length; i++) {
            assertEquals(-1, result[i]);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(CSetVar set, int offset, IntVar[] string, IntVar[] result) {
        return Constraints.filterString(set.getSet(), set.getCard(), offset, string, result);
    }
}
