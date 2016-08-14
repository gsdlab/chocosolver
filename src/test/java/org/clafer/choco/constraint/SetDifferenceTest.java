package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetDifferenceTest {

    @Input(solutions = 128)
    public Object testSetDifference(Model model) {
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
        return $(model.setVar("minuend", ker(), env(-2, -1, 0, 1)),
                model.setVar("subtrahend", ker(), env(-1, 0, 1, 2)),
                model.setVar("difference", ker(), env(-1, 0, 1, 2)));
    }

    @Check
    public void check(TIntSet minuend, TIntSet subtrahend, TIntSet difference) {
        TIntHashSet answer = new TIntHashSet(minuend);
        answer.removeAll(subtrahend);
        assertEquals(answer, difference);
    }

    @Test(timeout = 60000)
    public Constraint setup(SetVar minuend, SetVar subtrahend, SetVar difference) {
        return Constraints.difference(
                minuend, minuend.getCard(),
                subtrahend, subtrahend.getCard(),
                difference, difference.getCard());
    }
}
