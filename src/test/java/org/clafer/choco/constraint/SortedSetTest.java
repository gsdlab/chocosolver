package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.test.NonEmpty;
import org.chocosolver.solver.variables.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SortedSetTest {

    @Input(solutions = 20)
    public Object testSortedSet(Solver solver) {
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
        CSetVar[] sets = new CSetVar[3];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = cset("set" + i, 0, 2, solver);
        }
        return $(sets);
    }

    @Check
    public void check(int[][] sets) {
        int i = 0;
        for (int[] set : sets) {
            for (int j : set) {
                assertEquals(i++, j);
            }
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty CSetVar[] sets) {
        return Constraints.sortedSets(mapSet(sets), mapCard(sets));
    }
}
