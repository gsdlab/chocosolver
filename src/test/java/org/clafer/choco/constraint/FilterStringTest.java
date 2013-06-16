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
public class FilterStringTest extends ConstraintTest {

    private void checkCorrectness(SetVar set, int offset, IntVar[] string, IntVar[] result) {
        int[] $set = set.getValue();
        int[] $string = getValues(string);
        int[] $result = getValues(result);

        int i = 0;
        for (; i < $set.length; i++) {
            assertEquals($string[$set[i] - offset], $result[i]);
        }
        for (; i < $result.length; i++) {
            assertEquals(-1, $result[i]);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int offset = nextInt(5);
            int n = offset + nextInt(5) + 1;
            int lowChar = -nextInt(3);
            int highChar = nextInt(3);
            SetVar set = VF.set("set", Util.fromTo(offset, n), solver);

            IntVar[] string = VF.enumeratedArray("string", n, lowChar, highChar, solver);
            IntVar[] result = VF.enumeratedArray("result", nextInt(n) + 1, lowChar - 1, highChar, solver);

            solver.post(Constraints.filterString(set, offset, string, result));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(set, offset, string, result);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(set, offset, string, result);
            }
        }
    }

    @Test(timeout = 60000)
    public void testFilterString() {
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
        Solver solver = new Solver();

        SetVar set = VF.set("set", new int[]{0, 1, 2}, solver);
        int offset = 0;
        IntVar[] string = VF.enumeratedArray("string", 3, 0, 2, solver);
        IntVar[] result = VF.enumeratedArray("result", 3, -1, 2, solver);

        solver.post(Constraints.filterString(set, offset, string, result));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(set, offset, string, result);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(216, count);
    }
}
