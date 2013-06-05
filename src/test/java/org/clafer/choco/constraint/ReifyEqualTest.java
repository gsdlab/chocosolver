package org.clafer.choco.constraint;

import org.clafer.common.Util;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class ReifyEqualTest extends ConstraintTest {

    public void checkCorrectness(BoolVar reify, IntVar i, int c) {
        assertEquals(reify.getValue() == 1, i.getValue() == c);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar reify = VF.bool("reify", solver);
            IntVar i = VF.enumerated("i", Util.range(-nextInt(10), nextInt(10)), solver);
            int c = nextInt(20) - 10;

            solver.post(Constraints.reifyEqual(reify, i, c));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(reify, i, c);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(reify, i, c);
            }
        }
    }

    @Test(timeout = 60000)
    public void testReifyEqual() {
        Solver solver = new Solver();

        BoolVar reify = VF.bool("reify", solver);
        IntVar i = VF.enumerated("i", -10, 10, solver);
        int c = 4;

        solver.post(Constraints.reifyEqual(reify, i, c));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(reify, i, c);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(21, count);
    }
}
