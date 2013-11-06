package org.clafer.choco.constraint;

import org.clafer.common.Util;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class ReifyEqualTest extends ConstraintTest {

    public void checkCorrectness(BoolVar reify, int i, int j) {
        assertEquals(reify.getValue() == 1, i == j);
    }

    public void checkNotCorrectness(BoolVar reify, int i, int j) {
        assertEquals(reify.getValue() == 1, i != j);
    }

    @Test(timeout = 60000)
    public void quickTestXC() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar reify = VF.bool("reify", solver);
            IntVar i = VF.enumerated("i", Util.range(-nextInt(10), nextInt(10)), solver);
            int c = nextInt(20) - 10;

            solver.post(Constraints.reifyEqual(reify, i, c));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(reify, i.getValue(), c);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(reify, i.getValue(), c);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickNotTestXC() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar reify = VF.bool("reify", solver);
            IntVar i = VF.enumerated("i", Util.range(-nextInt(10), nextInt(10)), solver);
            int c = nextInt(20) - 10;

            solver.post(Constraints.reifyNotEqual(reify, i, c));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkNotCorrectness(reify, i.getValue(), c);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkNotCorrectness(reify, i.getValue(), c);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTestXY() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar reify = VF.bool("reify", solver);
            IntVar i = VF.enumerated("i", Util.range(-nextInt(3), nextInt(3)), solver);
            IntVar j = VF.enumerated("j", Util.range(-nextInt(3), nextInt(3)), solver);

            solver.post(Constraints.reifyEqual(reify, i, j));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(reify, i.getValue(), j.getValue());
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(reify, i.getValue(), j.getValue());
            }
        }
    }

    @Test(timeout = 60000)
    public void quickNotTestXY() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar reify = VF.bool("reify", solver);
            IntVar i = VF.enumerated("i", Util.range(-nextInt(3), nextInt(3)), solver);
            IntVar j = VF.enumerated("j", Util.range(-nextInt(3), nextInt(3)), solver);

            solver.post(Constraints.reifyNotEqual(reify, i, j));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkNotCorrectness(reify, i.getValue(), j.getValue());
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkNotCorrectness(reify, i.getValue(), j.getValue());
            }
        }
    }

    @Test(timeout = 60000)
    public void testReifyEqualXC() {
        Solver solver = new Solver();

        BoolVar reify = VF.bool("reify", solver);
        IntVar i = VF.enumerated("i", -10, 10, solver);
        int c = 4;

        solver.post(Constraints.reifyEqual(reify, i, c));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(reify, i.getValue(), c);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(21, count);
    }

    @Test(timeout = 60000)
    public void testReifyNotEqual() {
        Solver solver = new Solver();

        BoolVar reify = VF.bool("reify", solver);
        IntVar i = VF.enumerated("i", -10, 10, solver);
        int c = 4;

        solver.post(Constraints.reifyNotEqual(reify, i, c));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkNotCorrectness(reify, i.getValue(), c);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(21, count);
    }

    @Test(timeout = 60000)
    public void testReifyEqualXY() {
        Solver solver = new Solver();

        BoolVar reify = VF.bool("reify", solver);
        IntVar i = VF.enumerated("i", -2, 2, solver);
        IntVar j = VF.enumerated("j", -2, 2, solver);

        solver.post(Constraints.reifyEqual(reify, i, j));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(reify, i.getValue(), j.getValue());
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(25, count);
    }

    @Test(timeout = 60000)
    public void testReifyNotEqualXY() {
        Solver solver = new Solver();

        BoolVar reify = VF.bool("reify", solver);
        IntVar i = VF.enumerated("i", -2, 2, solver);
        IntVar j = VF.enumerated("j", -2, 2, solver);

        solver.post(Constraints.reifyNotEqual(reify, i, j));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkNotCorrectness(reify, i.getValue(), j.getValue());
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(25, count);
    }
}
