package org.clafer.choco.constraint;

import java.util.Arrays;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class SetNotEqualTest extends ConstraintTest {

    public void checkCorrectness(SetVar s1, SetVar s2) {
        int[] $s1 = s1.getValue();
        int[] $s2 = s2.getValue();

        assertFalse(Arrays.equals($s1, $s2));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar s1 = VF.set("s1", Util.range(-nextInt(10), nextInt(10)), solver);
            SetVar s2 = VF.set("s2", Util.range(-nextInt(10), nextInt(10)), solver);

            solver.post(Constraints.notEqual(s1, s2));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(s1, s2);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(s1, s2);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSetNotEqual() {
        /*
         * import Control.Monad
         * 
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-1..1]
         *     s2 <- powerset [-2..2]
         *     guard $ s1 /= s2
         *     return (s1, s2)
         */
        Solver solver = new Solver();

        SetVar s1 = VF.set("s1", Util.range(-1, 1), solver);
        SetVar s2 = VF.set("s2", Util.range(-2, 2), solver);

        solver.post(Constraints.notEqual(s1, s2));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(s1, s2);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(248, count);
    }
}
