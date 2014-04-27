package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.test.TestUtil.CSetVar;
import static org.junit.Assert.*;
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
public class SetDifferenceTest {

    @Input(solutions = 128)
    public Object testSetDifference(Solver solver) {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     minuend <- powerset [-2..1]
         *     subtrahend <- powerset [-1..2]
         *     difference <- powerset [-1..2]
         *     guard $ difference == deleteFirstsBy (==) minuend subtrahend
         *     return (minuend, subtrahend, difference)
         */
        return $(cset(",inuend", -2, 1, solver),
                cset("subtrahend", -1, 2, solver),
                cset("difference", -1, 2, solver));
    }

    @Check
    public void check(TIntSet minuend, TIntSet subtrahend, TIntSet difference) {
        TIntHashSet answer = new TIntHashSet(minuend);
        answer.removeAll(subtrahend);
        assertEquals(answer, difference);
    }

    @Test(timeout = 60000)
    public Constraint setup(CSetVar minuend, CSetVar subtrahend, CSetVar difference) {
        return Constraints.difference(
                minuend.getSet(), minuend.getCard(),
                subtrahend.getSet(), subtrahend.getCard(),
                difference.getSet(), difference.getCard());
    }
}
