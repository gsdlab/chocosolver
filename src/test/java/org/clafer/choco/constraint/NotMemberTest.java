package org.clafer.choco.constraint;

import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class NotMemberTest extends ConstraintTest {

    private void checkCorrectness(IntVar element, SetVar set) {
        assertFalse(Util.in(element.getValue(), set.getValue()));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int num = nextInt(10);

            IntVar element = VF.enumerated("element", -nextInt(10), nextInt(10), solver);
            SetVar set = VF.set("set", Util.range(-nextInt(10), nextInt(10)), solver);

            solver.post(Constraints.notMember(element, set));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(element, set);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(element, set);
            }
        }
    }

    @Test(timeout = 60000)
    public void testNotMember() {
        Solver solver = new Solver();

        IntVar element = VF.enumerated("element", -1, 3, solver);
        SetVar set = VF.set("set", new int[]{0, 1, 2, 3, 4, 5}, solver);

        solver.post(Constraints.notMember(element, set));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(element, set);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(192, count);
    }
}
