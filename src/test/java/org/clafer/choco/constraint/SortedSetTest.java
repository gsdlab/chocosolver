package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import org.chocosolver.solver.variables.CSetVar;
import static org.junit.Assert.*;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
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
        return $(csetArray("set", 3, 0, 2, solver),
                enumeratedArray("bound", 3, 0, 3, solver));
    }

    @Check
    public void check(int[][] sets, int[] bounds) {
        for (int i = 0; i < bounds.length; i++) {
            int from = i == 0 ? 0 : bounds[i - 1];
            int to = bounds[i];
            assertTrue(from <= to);
            int[] set = sets[i];
            assertArrayEquals(Util.fromTo(from, to), set);
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty CSetVar[] sets, @NonEmpty IntVar[] bounds) {
        assumeTrue(sets.length == bounds.length);
        return Constraints.sortedSets(mapSet(sets), mapCard(sets), bounds);
    }
}
