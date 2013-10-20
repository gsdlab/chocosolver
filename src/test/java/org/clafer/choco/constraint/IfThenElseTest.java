package org.clafer.choco.constraint;

import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class IfThenElseTest extends ConstraintTest {

    private void checkCorrectness(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        if (antecedent.instantiatedTo(1)) {
            assertTrue(antecedent + " : " + consequent + " : " + alternative, consequent.instantiatedTo(1));
        } else {
            assertTrue(alternative.instantiatedTo(1));
        }
    }

    private BoolVar randBoolVar(String name, Solver solver) {
        switch (nextInt(5)) {
            case 0:
                return solver.ZERO;
            case 1:
                return solver.ONE;
            default:
                return VF.bool(name, solver);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            BoolVar antecedent = randBoolVar("antecedent", solver);
            BoolVar consequent = randBoolVar("consequent", solver);
            BoolVar alternative = randBoolVar("alternative", solver);

            solver.post(Constraints.ifThenElse(antecedent, consequent, alternative));

            if (randomizeStrategy(solver).findSolution()) {
                checkCorrectness(antecedent, consequent, alternative);
                int solutions;
                for (solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                    checkCorrectness(antecedent, consequent, alternative);
                }
                assertTrue(solutions <= 4);
            }
        }
    }

    @Test(timeout = 60000)
    public void testIfThenElse() {
        Solver solver = new Solver();

        BoolVar antecedent = VF.bool("antecedent", solver);
        BoolVar consequent = VF.bool("consequent", solver);
        BoolVar alternative = VF.bool("alternative", solver);

        solver.post(Constraints.ifThenElse(antecedent, consequent, alternative));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(antecedent, consequent, alternative);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(4, count);
    }
}
