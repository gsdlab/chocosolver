package org.clafer.constraint;

import org.clafer.Util;
import org.clafer.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class IntChannelTest extends ConstraintTest {

    private void checkCorrectness(SetVar[] sets, IntVar[] ints) {
        int[][] $sets = PropUtil.getValues(sets);
        int[] $ints = PropUtil.getValues(ints);

        for (int i = 0; i < $sets.length; i++) {
            for (int j : $sets[i]) {
                assertEquals(i, $ints[j]);
            }
        }
        for (int i = 0; i < $ints.length; i++) {
            assertTrue(Util.in(i, $sets[$ints[i]]));
        }
    }

    @Test(timeout = 60000)
    public void testIntChannel() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int x = nextInt(10) + 1;
            int y = nextInt(10) + 1;

            SetVar[] sets = new SetVar[x];
            for (int i = 0; i < sets.length; i++) {
                sets[i] = VariableFactory.set("set_" + i, Util.fromTo(0, y), solver);
            }
            IntVar[] ints = VariableFactory.enumeratedArray("int", y, Util.fromTo(0, x), solver);

            solver.post(Constraints.intChannel(sets, ints));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(sets, ints);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(sets, ints);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        SetVar[] sets = new SetVar[3];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = VariableFactory.set("set_" + i, Util.fromTo(0, 5), solver);
        }
        IntVar[] ints = VariableFactory.enumeratedArray("int", 5, Util.fromTo(0, 3), solver);

        solver.post(Constraints.intChannel(sets, ints));

        assertEquals(243, randomizeStrategy(solver).findAllSolutions());
    }
}
