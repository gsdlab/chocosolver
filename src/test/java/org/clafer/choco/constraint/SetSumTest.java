package org.clafer.choco.constraint;

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
public class SetSumTest extends ConstraintTest {

    private void checkCorrectness(SetVar set, IntVar setCard, IntVar sum) {
        int[] $set = set.getValue();
        int $sum = sum.getValue();

        assertTrue($set.length <= setCard.getValue());

        int sumSet = 0;
        for (int s : $set) {
            sumSet += s;
        }
        assertEquals(sumSet, $sum);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar set = VF.set("set", -nextInt(10), nextInt(10), solver);
            IntVar setCard = VF.enumerated("|set|", 0, nextInt(10) + 1, solver);
            IntVar sum = VF.enumerated("sum", -nextInt(100), nextInt(100), solver);

            solver.post(Constraints.setSum(set, setCard, sum));

            assertTrue(solver.toString(), randomizeStrategy(solver).findSolution());
            checkCorrectness(set, setCard, sum);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(set, setCard, sum);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSumSet() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     set <- filterM (const $ [True, False]) [-4..5]
         *     guard $ length set <= 7
         *     guard $ -120 <= sum set && sum set <= 120
         *     return set
         */
        Solver solver = new Solver();

        SetVar set = VF.set("set", -4, 5, solver);
        IntVar setCard = VF.enumerated("|set|", 0, 7, solver);
        IntVar sum = VF.enumerated("sum", -120, 120, solver);

        solver.post(Constraints.setSum(set, setCard, sum));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(set, setCard, sum);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(968, count);
    }

    @Test(timeout = 60000)
    public void testSumNonPositiveSet() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     set <- filterM (const $ [True, False]) [-7..0]
         *     guard $ -4 <= sum set && sum set <= 13
         *     return set
         */
        Solver solver = new Solver();

        SetVar set = VF.set("set", -7, 0, solver);
        IntVar setCard = VF.enumerated("|set|", 0, 8, solver);
        IntVar sum = VF.enumerated("sum", -4, 13, solver);

        solver.post(Constraints.setSum(set, setCard, sum));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(set, setCard, sum);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(14, count);
    }

    @Test(timeout = 60000)
    public void testPartiallySolved() {
        Solver solver = new Solver();

        SetVar set = VF.set("set", -4, 0, solver);
        IntVar setCard = VF.enumerated("|set|", 3, 5, solver);
        IntVar sum = VF.enumerated("sum", -4, -1, solver);

        solver.post(Constraints.setSum(set, setCard, sum));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(set, setCard, sum);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(2, count);
    }

    @Test
    public void testSumKnown() {
        Solver solver = new Solver();

        SetVar set = VF.set("set", 0, 10, solver);
        IntVar setCard = VF.enumerated("|set|", 1, 2, solver);
        IntVar sum = VF.enumerated("sum", 2, 2, solver);

        solver.post(Constraints.setSum(set, setCard, sum));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(set, setCard, sum);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(2, count);
    }
}
