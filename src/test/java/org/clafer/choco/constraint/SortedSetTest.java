package org.clafer.choco.constraint;

import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class SortedSetTest extends ConstraintTest {

    private void checkCorrectness(SetVar[] sets) {
        int i = 0;
        for (SetVar set : sets) {
            for (int j = set.getKernelFirst(); j != SetVar.END; j = set.getKernelNext()) {
                assertEquals(i++, j);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 1; repeat++) {
            Solver solver = new Solver();

            SetVar[] sets = new SetVar[nextInt(5) + 1];
            IntVar[] cards = new IntVar[sets.length];
            for (int i = 0; i < sets.length; i++) {
                int low = nextInt(3);
                int high = low + nextInt(10);
                sets[i] = VF.set("set" + i, low, high, solver);
                cards[i] = VF.enumerated("|set" + i + "|", 0, high - low + 1, solver);
                solver.post(SCF.cardinality(sets[i], cards[i]));
            }

            solver.post(Constraints.sortedSets(sets, cards));

            assertTrue(solver.toString(), randomizeStrategy(solver).findSolution());
            checkCorrectness(sets);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(sets);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSortedSet() {
        /*
         * import Control.Monad
         * 
         * solutions = do
         *     a <- [0..10]
         *     b <- [0..10]
         *     c <- [0..10]
         *     guard $ a + b + c <= 10
         *     return (a, b, c)
         */
        Solver solver = new Solver();

        SetVar[] sets = new SetVar[3];
        IntVar[] cards = new IntVar[sets.length];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = VF.set("set" + i, 0, 9, solver);
            cards[i] = VF.enumerated("|set" + i + "|", 0, 10, solver);
            solver.post(SCF.cardinality(sets[i], cards[i]));
        }

        solver.post(Constraints.sortedSets(sets, cards));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(sets);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(286, count);
    }
}
