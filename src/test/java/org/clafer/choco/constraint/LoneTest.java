package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class LoneTest extends ConstraintTest {

    private void checkCorrectness(BoolVar[] vars) {
        int[] $vars = PropUtil.getValues(vars);
        int count = 0;
        for (int var : $vars) {
            if (var == 1) {
                count++;
            }
        }
        assertTrue(count <= 1);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar[] vars = VF.boolArray("var", nextInt(100) + 1, solver);

            solver.post(Constraints.lone(vars));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(vars);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(vars);
            }
        }
    }

    @Test(timeout = 60000)
    public void testLone() {
        Solver solver = new Solver();

        BoolVar[] vars = VF.boolArray("var", 5, solver);

        solver.post(Constraints.lone(vars));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(vars);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(6, count);
    }
}