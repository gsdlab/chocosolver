package org.clafer.choco.constraint;

import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class SetEqualTest extends ConstraintTest {

    public void checkCorrectness(SetVar s1, SetVar s2) {
        int[] $s1 = s1.getValue();
        int[] $s2 = s2.getValue();

        assertArrayEquals($s1, $s2);
    }

    @Test(timeout = 60000)
    public void testSetEqual() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar s1 = VariableFactory.set("s1", Util.range(-nextInt(10), nextInt(10)), solver);
            SetVar s2 = VariableFactory.set("s2", Util.range(-nextInt(10), nextInt(10)), solver);

            solver.post(Constraints.equal(s1, s2));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(s1, s2);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(s1, s2);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        SetVar s1 = VariableFactory.set("s1", Util.range(-5, 10), solver);
        SetVar s2 = VariableFactory.set("s2", Util.range(-10, 5), solver);

        solver.post(Constraints.equal(s1, s2));

        assertEquals(2048, randomizeStrategy(solver).findAllSolutions());
    }
}
