package org.clafer.constraint;

import org.clafer.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class IncreasingTest extends ConstraintTest {

    private void checkCorrectness(IntVar[] vars) {
        int[] $vars = PropUtil.getValues(vars);

        for (int i = 1; i < vars.length; i++) {
            assertTrue($vars[i - 1] <= $vars[i]);
        }
    }

    @Test(timeout = 60000)
    public void testIncreasing() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int sup = nextInt(10) + 1;
            int inf = nextInt(sup);

            IntVar[] vars = VariableFactory.enumeratedArray("var", nextInt(10) + 1, inf, sup, solver);

            solver.post(Constraints.increasing(vars));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(vars);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(vars);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        IntVar[] vars = VariableFactory.enumeratedArray("var", 5, 0, 10, solver);

        solver.post(Constraints.increasing(vars));

        assertEquals(3003, randomizeStrategy(solver).findAllSolutions());
    }
}
