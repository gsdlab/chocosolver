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
public class MaskTest extends ConstraintTest {

    private void checkCorrectness(SetVar[] sets) {
        int i = 0;
        for (SetVar set : sets) {
            for (int j = set.getKernelFirst(); j != SetVar.END; j = set.getKernelNext()) {
                assertEquals(i++, j);
            }
        }
    }

    private void checkCorrectness(SetVar set, SetVar masked, int from, int to) {
        int count = 0;
        for (int i = 0; i < to - from; i++) {
            assertEquals(set.kernelContains(from + i), masked.kernelContains(i));
            count += masked.kernelContains(i) ? 1 : 0;
        }
        assertEquals(count, masked.getKernelSize());
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 1; repeat++) {
            Solver solver = new Solver();

            int setLow = nextInt(5);
            int setHigh = setLow + nextInt(5);
            int maskedLow = nextInt(5);
            int maskedHigh = maskedLow + nextInt(5);
            int from = nextInt(3);
            int to = from + nextInt(5);
            
            SetVar set = VF.set("set", setLow, setHigh, solver);
            IntVar setCard = cardVar(set);
            solver.post(SCF.cardinality(set, setCard));
            SetVar masked = VF.set("masked", maskedLow, maskedHigh, solver);
            IntVar maskedCard = cardVar(masked);

            solver.post(Constraints.mask(set, setCard, masked, maskedCard, from, to));

            assertTrue(solver.toString(), randomizeStrategy(solver).findSolution());
            checkCorrectness(set, masked, from, to);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(set, masked, from, to);
            }
        }
    }

    @Test(timeout = 60000)
    public void testMask() {
        /*
         * import Control.Monad
         * 
         * solutions = filterM (const [True, False]) [1..6]
         */
        Solver solver = new Solver();

        SetVar set = VF.set("set", 1, 6, solver);
        IntVar setCard = cardVar(set);
        solver.post(SCF.cardinality(set, setCard));
        SetVar masked = VF.set("masked", 0, 3, solver);
        IntVar maskedCard = cardVar(masked);

        int from = 2;
        int to = 5;

        solver.post(Constraints.mask(set, setCard, masked, maskedCard, from, to));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(set, masked, from, to);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(64, count);
    }
}
