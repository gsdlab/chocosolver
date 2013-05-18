package org.clafer.choco.constraint;

import org.clafer.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class SumSetNTest extends ConstraintTest {

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
    public void testSumSetN() throws Throwable {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar set = VariableFactory.set("set", Util.range(-nextInt(10), nextInt(10)), solver);
            IntVar sum = VariableFactory.enumerated("sum", -nextInt(100), nextInt(100), solver);
            int n = nextInt(10) + 1;

            solver.post(Constraints.sumSetN(set, sum, n));

            assertTrue(solver.toString(), randomizeStrategy(solver).findSolution());
            checkCorrectness(set, sum, n);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(set, sum, n);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        SetVar set = VariableFactory.set("set", Util.range(-4, 5), solver);
        IntVar sum = VariableFactory.enumerated("sum", -120, 120, solver);

        solver.post(Constraints.sumSetN(set, sum, 7));

        assertEquals(968, randomizeStrategy(solver).findAllSolutions());
    }
}
