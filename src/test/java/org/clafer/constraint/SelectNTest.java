package org.clafer.constraint;

import org.clafer.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class SelectNTest extends ConstraintTest {

    private void checkCorrectness(BoolVar[] bools, IntVar n) {
        int[] $bools = PropUtil.getValues(bools);
        int $n = n.getValue();

        for (int i = 0; i < $n; i++) {
            assertEquals(1, $bools[i]);
        }
        for (int i = $n; i < $bools.length; i++) {
            assertEquals(0, $bools[i]);
        }
    }

    @Test(timeout = 60000)
    public void testSelectN() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int num = nextInt(100);

            BoolVar[] bools = VariableFactory.boolArray("bool", num, solver);
            IntVar n = VariableFactory.enumerated("n", 0, num, solver);

            solver.post(Constraints.selectN(bools, n));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(bools, n);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(bools, n);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        BoolVar[] bools = VariableFactory.boolArray("bool", 20, solver);
        IntVar n = VariableFactory.enumerated("n", 0, 20, solver);

        solver.post(Constraints.selectN(bools, n));

        assertEquals(21, randomizeStrategy(solver).findAllSolutions());
    }
}
