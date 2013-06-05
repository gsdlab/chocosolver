package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class SetDifferenceTest extends ConstraintTest {

    public void checkCorrectness(SetVar minuend, SetVar subtrahend, SetVar difference) {
        int[] $minuend = minuend.getValue();
        int[] $subtrahend = subtrahend.getValue();
        int[] $difference = difference.getValue();

        TIntHashSet answer = new TIntHashSet($minuend);
        answer.removeAll($subtrahend);
        assertEquals(answer, new TIntHashSet($difference));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar minuend = VF.set("Minuend", Util.range(-nextInt(10), nextInt(10)), solver);
            SetVar subtrahend = VF.set("Subtrahend", Util.range(-nextInt(10), nextInt(10)), solver);
            SetVar difference = VF.set("Difference", Util.range(-nextInt(10), nextInt(10)), solver);

            solver.post(Constraints.difference(minuend, subtrahend, difference));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(minuend, subtrahend, difference);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(minuend, subtrahend, difference);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSetDifference() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     minuend <- powerset [-2..4]
         *     subtrahend <- powerset [-4..2]
         *     difference <- powerset [-1..3]
         *     guard $ difference == deleteFirstsBy (==) minuend subtrahend
         *     return (minuend, subtrahend, difference)
         */
        Solver solver = new Solver();

        SetVar minuend = VF.set("Minuend", Util.range(-2, 4), solver);
        SetVar subtrahend = VF.set("Subtrahend", Util.range(-4, 2), solver);
        SetVar difference = VF.set("Difference", Util.range(-1, 3), solver);

        solver.post(Constraints.difference(minuend, subtrahend, difference));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(minuend, subtrahend, difference);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(6144, count);
    }
}
