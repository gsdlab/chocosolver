package org.clafer.choco.constraint;

import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class SetSumNTest extends ConstraintTest {

    private void checkCorrectness(SetVar set, IntVar sum, int n) {
        int[] $set = set.getValue();
        int $sum = sum.getValue();

        assertTrue($set.length <= n);

        int sumSet = 0;
        for (int s : $set) {
            sumSet += s;
        }
        assertEquals(sumSet, $sum);
    }

    @Test(timeout = 60000)
    public void quickTest() throws Throwable {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar set = VF.set("set", Util.range(-nextInt(10), nextInt(10)), solver);
            IntVar sum = VF.enumerated("sum", -nextInt(100), nextInt(100), solver);
            int n = nextInt(10) + 1;

            solver.post(Constraints.setSumN(set, sum, n));

            assertTrue(solver.toString(), randomizeStrategy(solver).findSolution());
            checkCorrectness(set, sum, n);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(set, sum, n);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSumSetN() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     set <- filterM (const $ [True, False]) [-4..5]
         *     guard $ length set <= 7
         *     guard $ -120 <= sum set && sum set <= 120
         *     return set
         */
        Solver solver = new Solver();

        SetVar set = VF.set("set", Util.range(-4, 5), solver);
        IntVar sum = VF.enumerated("sum", -120, 120, solver);

        solver.post(Constraints.setSumN(set, sum, 7));

        assertEquals(968, randomizeStrategy(solver).findAllSolutions());
    }

    @Test
    public void testSumNonPositiveSet() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     set <- filterM (const $ [True, False]) [-7..0]
         *     guard $ -4 <= sum set && sum set <= 13
         *     return set
         */
        Solver solver = new Solver();

        SetVar set = VF.set("set", Util.range(-7, 0), solver);
        IntVar sum = VF.enumerated("sum", -4, 13, solver);
        int n = 8;

        solver.post(Constraints.setSumN(set, sum, n));

        assertEquals(14, randomizeStrategy(solver).findAllSolutions());
    }
}
