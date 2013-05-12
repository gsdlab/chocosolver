package org.clafer.constraint;

import org.clafer.Util;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import org.junit.Test;
import solver.variables.VariableFactory;
import static org.junit.Assert.*;

/**
 *
 * @author jimmy
 */
public class SingletonTest extends ConstraintTest {

    private void checkCorrectness(IntVar i, SetVar s) {
        int $i = i.getValue();
        int[] $s = s.getValue();

        assertEquals(1, $s.length);
        assertEquals($i, $s[0]);
    }

    @Test(timeout = 60000)
    public void testSingleton() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            IntVar i = VariableFactory.enumerated("i", -nextInt(1000), nextInt(1000), solver);
            SetVar s = VariableFactory.set("s", Util.range(-nextInt(1000), nextInt(1000)), solver);

            solver.post(Constraints.singleton(i, s));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(i, s);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(i, s);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        IntVar i = VariableFactory.enumerated("i", -120, 10, solver);
        SetVar s = VariableFactory.set("s", Util.range(-10, 111), solver);

        solver.post(Constraints.singleton(i, s));

        assertEquals(21, randomizeStrategy(solver).findAllSolutions());
    }
}
