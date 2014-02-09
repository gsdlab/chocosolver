package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class LengthTest extends ConstraintTest<Pair<IntVar, IntVar[]>> {

    @Override
    protected void check(Pair<IntVar, IntVar[]> s) {
        int length = s.getFst().getValue();
        int[] chars = getValues(s.getSnd());
        assertTrue(length <= chars.length);
        for (int i = 0; i < length; i++) {
            assertNotEquals(0, chars[i]);
        }
        for (int i = length; i < chars.length; i++) {
            assertEquals(0, chars[i]);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<IntVar, IntVar[]>>() {
            @Override
            public Pair<Constraint, Pair<IntVar, IntVar[]>> setup(Solver solver) {
                IntVar length = toIntVar(randPositiveInt(), solver);
                IntVar[] chars = toIntVars(randPositiveInts(3), solver);
                Constraint constraint = Constraints.length(length, chars);
                return pair(constraint, pair(length, chars));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLength() {
        randomizedTest(new TestCase<Pair<IntVar, IntVar[]>>() {
            /*
             * import Control.Monad
             *
             * positive = [0..3] >>= flip replicateM [1..4]
             * negative = length (liftM2 (,) [0..3] $ replicateM 3 [0..4]) - length positive 
             */
            @PositiveSolutions(85)
            @NegativeSolutions(415)
            @Override
            public Pair<Constraint, Pair<IntVar, IntVar[]>> setup(Solver solver) {
                IntVar length = VF.enumerated("length", 0, 3, solver);
                IntVar[] chars = VF.enumeratedArray("char", 3, 0, 4, solver);
                Constraint constraint = Constraints.length(length, chars);
                return pair(constraint, pair(length, chars));
            }
        });
    }
}
