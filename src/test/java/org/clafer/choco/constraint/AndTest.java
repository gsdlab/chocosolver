package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropUtil;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class AndTest extends ConstraintTest {

    private void checkCorrectness(BoolVar[] vars) {
        int[] $vars = PropUtil.getValues(vars);
        for (int var : $vars) {
            if (var == 0) {
                fail("Not all true.");
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar[] vars = VF.boolArray("var", nextInt(100) + 1, solver);

            solver.post(Constraints.and(vars));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(vars);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(vars);
            }
        }
    }

    @Test(timeout = 60000)
    public void testAnd() {
        Solver solver = new Solver();

        BoolVar[] vars = VF.boolArray("var", 5, solver);

        solver.post(Constraints.and(vars));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(vars);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(1, count);
    }
}
