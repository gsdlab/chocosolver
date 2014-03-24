package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
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
public class SortedSetTest extends ConstraintTest<SetVar[]> {

    @Override
    protected void check(SetVar[] sets) {
        int i = 0;
        for (SetVar set : sets) {
            for (int j = set.getKernelFirst(); j != SetVar.END; j = set.getKernelNext()) {
                assertEquals(i++, j);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<SetVar[]>() {
            @Override
            public Pair<Constraint, SetVar[]> setup(Solver solver) {
                CSetVar[] sets = toVars(randPositiveSets(nextInt(2) + 1), solver);
                return pair(Constraints.sortedSets(mapSet(sets), mapCard(sets)),
                        mapSet(sets));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSortedSet() {
        randomizedTest(new TestCase<SetVar[]>() {
            /*
             * import Control.Monad
             *         
             * positive = do
             *     a <- [0..3]
             *     b <- [0..3]
             *     c <- [0..3]
             *     guard $ a + b + c <= 3
             *     return (a, b, c)
             *   
             *negative = 2^3 * 2^3 * 2^3 - length positive
             */
            @PositiveSolutions(20)
            @NegativeSolutions(492)
            @Override
            public Pair<Constraint, SetVar[]> setup(Solver solver) {
                SetVar[] sets = new SetVar[3];
                for (int i = 0; i < sets.length; i++) {
                    sets[i] = VF.set("set" + i, 0, 2, solver);
                }
                IntVar[] cards = enforcedCardVars(sets);
                return pair(Constraints.sortedSets(sets, cards), sets);
            }
        });
    }
}
