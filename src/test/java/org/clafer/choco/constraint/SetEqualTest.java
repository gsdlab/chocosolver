package org.clafer.choco.constraint;

import org.clafer.common.Util;
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
public class SetEqualTest extends ConstraintTest {

    public void checkCorrectness(SetVar s1, SetVar s2) {
        int[] $s1 = s1.getValue();
        int[] $s2 = s2.getValue();

        assertArrayEquals($s1, $s2);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar s1 = VF.set("s1", Util.range(-nextInt(10), nextInt(10)), solver);
            IntVar s1Card = VF.enumerated("|s1|", 0, s1.getEnvelopeSize(), solver);
            SetVar s2 = VF.set("s2", Util.range(-nextInt(10), nextInt(10)), solver);
            IntVar s2Card = VF.enumerated("|s2|", 0, s2.getEnvelopeSize(), solver);

            solver.post(SCF.cardinality(s1, s1Card));
            solver.post(SCF.cardinality(s2, s2Card));
            solver.post(Constraints.equal(s1, s1Card, s2, s2Card));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(s1, s2);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(s1, s2);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSetEqual() {
        Solver solver = new Solver();

        SetVar s1 = VF.set("s1", Util.range(-5, 10), solver);
        IntVar s1Card = VF.enumerated("|s1|", 0, s1.getEnvelopeSize(), solver);
        SetVar s2 = VF.set("s2", Util.range(-10, 5), solver);
        IntVar s2Card = VF.enumerated("|s2|", 0, s2.getEnvelopeSize(), solver);

        solver.post(SCF.cardinality(s1, s1Card));
        solver.post(SCF.cardinality(s2, s2Card));
        solver.post(Constraints.equal(s1, s1Card, s2, s2Card));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(s1, s2);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(2048, count);
    }
}
