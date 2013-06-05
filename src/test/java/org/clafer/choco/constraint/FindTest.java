package org.clafer.choco.constraint;

import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class FindTest extends ConstraintTest {

    private void checkCorrectness(int value, IntVar[] array, IntVar index) {
        for (int i = 0; i < index.getValue() - 1; i++) {
            assertNotEquals(value, array[i].getValue());
        }
        assertEquals(value, array[index.getValue()].getValue());
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int n = nextInt(10);
            int value = 0;
            IntVar[] array = VF.enumeratedArray("array", n + 1, -nextInt(10), nextInt(10), solver);
            IntVar index = VF.enumerated("index", 0, n, solver);

            solver.post(Constraints.find(value, array, index));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(value, array, index);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(value, array, index);
            }
        }
    }

    @Test(timeout = 60000)
    public void testFind() {
        for (int i = 0; i < 100; i++) {
            Solver solver = new Solver();

            int value = 1;
            IntVar[] array = VF.enumeratedArray("array", 3, -1, 1, solver);
            IntVar index = VF.enumerated("index", 0, 2, solver);

            solver.post(Constraints.find(value, array, index));

            int count = 0;
            if (randomizeStrategy(solver).findSolution()) {
                do {
                    checkCorrectness(value, array, index);
                    count++;
                } while (solver.nextSolution());
            }
            assertEquals(19, count);
        }
    }
}
