package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class FilterStringTest extends ConstraintTest<Triple<SetVar, Integer, Pair<IntVar[], IntVar[]>>> {

    @Override
    protected void check(Triple<SetVar, Integer, Pair<IntVar[], IntVar[]>> s) {
        int[] set = s.getFst().getValue();
        int offset = s.getSnd();
        int[] string = getValues(s.getThd().getFst());
        int[] result = getValues(s.getThd().getSnd());

        int i = 0;
        for (; i < set.length; i++) {
            assertTrue(set[i] - offset >= 0);
            assertTrue(set[i] - offset < string.length);
            assertTrue(i >= 0);
            assertTrue(i < result.length);
            assertEquals(string[set[i] - offset], result[i]);
        }
        for (; i < result.length; i++) {
            assertEquals(-1, result[i]);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<SetVar, Integer, Pair<IntVar[], IntVar[]>>>() {
            @Override
            public Pair<Constraint, Triple<SetVar, Integer, Pair<IntVar[], IntVar[]>>> setup(Solver solver) {
                SetVar set = toSetVar(randSet(), solver);
                int offset = nextInt(5);
                IntVar[] string = toIntVars(randInts(nextInt(3) + 1), solver);
                IntVar[] result = toIntVars(randInts(nextInt(3) + 1), solver);
                return pair(Constraints.filterString(set, offset, string, result),
                        triple(set, offset, pair(string, result)));
            }
        });
    }

    @Test(timeout = 60000)
    public void testFilterString() {
        randomizedTest(new TestCase<Triple<SetVar, Integer, Pair<IntVar[], IntVar[]>>>() {
            /*
             * import Control.Monad
             * 
             * powerset = filterM (const [True, False])
             * 
             * positive = do
             *     set <- powerset [0..2]
             *     string <- sequence $ replicate 3 [0..2]
             *     let result = [string !! i | i <- set]
             * 
             *     return (set, string, result)
             * 
             * negative = 2^3*3^3*4^3 - length positive
             */
            @PositiveSolutions(216)
            @NegativeSolutions(13608)
            @Override
            public Pair<Constraint, Triple<SetVar, Integer, Pair<IntVar[], IntVar[]>>> setup(Solver solver) {
                SetVar set = VF.set("set", 0, 2, solver);
                int offset = 0;
                IntVar[] string = VF.enumeratedArray("string", 3, 0, 2, solver);
                IntVar[] result = VF.enumeratedArray("result", 3, -1, 2, solver);

                return pair(Constraints.filterString(set, offset, string, result),
                        triple(set, offset, pair(string, result)));
            }
        });
    }
}
