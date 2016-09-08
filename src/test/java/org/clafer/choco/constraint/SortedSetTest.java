package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.chocosolver.solver.variables.Var.mapCard;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SortedSetTest {

    @Input(solutions = 20)
    public Object testSortedSet(Model model) {
        /*
         * import Control.Monad
         *
         * positive = do
         *     a <- [0..3]
         *     b <- [0..3]
         *     c <- [0..3]
         *     guard $ a + b + c <= 3
         *     return (a, b, c)
         */
        return $(model.setVarArray("set", 3, ker(), env(0, 1, 2)),
                model.intVarArray("bound", 3, 0, 3));
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
    public Constraint setup(@NonEmpty SetVar[] sets, @NonEmpty IntVar[] bounds) {
        assumeTrue(sets.length == bounds.length);
        return Constraints.sortedSets(sets, mapCard(sets), bounds);
    }
}
