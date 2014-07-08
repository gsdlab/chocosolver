package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import solver.variables.CSetVar;
import static org.junit.Assert.*;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class MaskTest {

    @Input(solutions = 64)
    public Object testMask(Solver solver) {
        /*
         * import Control.Monad
         * import Data.List
         *         
         * powerset = filterM (const [True, False])
         *         
         * solutions = do
         *     set <- powerset [1..6]
         *     masked <- powerset [0..3]
         *     guard $ masked == [s - 2 | s <- set, s >= 2 && s < 5]
         *     return (set, masked)
         */
        return $(cset("set", 1, 6, solver),
                cset("masked", 0, 3, solver),
                2, 5);
    }

    @Check
    public void check(TIntSet set, TIntSet masked, int from, int to) {
        int count = 0;
        for (int i = 0; i < to - from; i++) {
            assertEquals(set.contains(from + i), masked.contains(i));
            count += masked.contains(i) ? 1 : 0;
        }
        assertEquals(count, masked.size());
    }

    @Test(timeout = 60000)
    public Constraint setup(CSetVar set, CSetVar masked, int from, int to) {
        assumeTrue(from <= to);
        return Constraints.mask(set.getSet(), set.getCard(),
                masked.getSet(), masked.getCard(), from, to);
    }
}
