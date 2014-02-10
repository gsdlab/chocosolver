package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
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
public class SamePrefixTest extends ConstraintTest<Triple<IntVar, IntVar[], IntVar[]>> {

    @Override
    protected void check(Triple<IntVar, IntVar[], IntVar[]> s) {
        int length = s.getFst().getValue();
        int[] string1 = getValues(s.getSnd());
        int[] string2 = getValues(s.getThd());
        for (int i = 0; i < length; i++) {
            assertEquals(string1[i], string2[i]);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<IntVar, IntVar[], IntVar[]>>() {
            @Override
            public Pair<Constraint, Triple<IntVar, IntVar[], IntVar[]>> setup(Solver solver) {
                IntVar length = toIntVar(randPositiveInt(), solver);
                IntVar[] string1 = toIntVars(randInts(3), solver);
                IntVar[] string2 = toIntVars(randInts(4), solver);
                Constraint constraint = Constraints.samePrefix(length, string1, string2);
                return pair(constraint, triple(length, string1, string2));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSamePrefix() {
        randomizedTest(new TestCase<Triple<IntVar, IntVar[], IntVar[]>>() {
            /*
             * import Control.Monad
             *
             * positive = do
             *     length <- [0..3]
             *     string1 <- replicateM 3 ['a', 'b', 'c']
             *     string2 <- replicateM 3 ['a', 'b', 'c']
             *     guard $ take length string1 == take length string2
             *     return (length, string1, string2)
             * negative = length (liftM3 (,,) [0..3] (replicateM 3 ['a', 'b', 'c']) (replicateM 3 ['a', 'b', 'c'])) - length positive
             */
            @PositiveSolutions(1080)
            @NegativeSolutions(1836)
            @Override
            public Pair<Constraint, Triple<IntVar, IntVar[], IntVar[]>> setup(Solver solver) {
                IntVar length = VF.enumerated("length", 0, 3, solver);
                IntVar[] string1 = VF.enumeratedArray("string1", 3, 0, 2, solver);
                IntVar[] string2 = VF.enumeratedArray("string2", 3, 0, 2, solver);
                Constraint constraint = Constraints.samePrefix(length, string1, string2);
                return pair(constraint, triple(length, string1, string2));
            }
        });
    }
}
