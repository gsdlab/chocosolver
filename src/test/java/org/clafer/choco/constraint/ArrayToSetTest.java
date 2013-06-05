package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.common.Util;
import org.clafer.choco.constraint.propagator.PropUtil;
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
public class ArrayToSetTest extends ConstraintTest {

    private void checkCorrectness(IntVar[] array, SetVar set) {
        int[] $array = PropUtil.getValues(array);
        int[] $set = set.getValue();

        assertEquals(new TIntHashSet($array), new TIntHashSet($set));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int low = nextInt(5) + 1;
            int high = nextInt(5);

            IntVar[] array = VF.enumeratedArray("array", low + high, -low - nextInt(10), high + nextInt(10), solver);
            SetVar set = VF.set("set", Util.range(-low - nextInt(10), high + nextInt(10)), solver);

            solver.post(Constraints.arrayToSet(array, set));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(array, set);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(array, set);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTestLargeDomain() {
        Solver solver = new Solver();

        IntVar[] array = VF.boundedArray("array", 5, 0, 100000, solver);
        SetVar set = VF.set("set", Util.range(0, 10000), solver);

        solver.post(Constraints.arrayToSet(array, set));

        assertTrue(randomizeStrategy(solver).findSolution());
        checkCorrectness(array, set);
    }

    @Test(timeout = 60000)
    public void testArrayToSet() {
        Solver solver = new Solver();

        IntVar[] ivars = VF.enumeratedArray("ivar", 3, 0, 5, solver);
        SetVar svar = VF.set("svar", new int[]{0, 1, 2, 3, 4, 5}, solver);

        solver.post(Constraints.arrayToSet(ivars, svar));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(ivars, svar);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(216, count);
    }
}
