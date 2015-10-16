package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CSetVar;
import static org.chocosolver.solver.variables.Var.*;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropContainsImpliesEqual;
import org.clafer.choco.constraint.propagator.PropContainsImpliesEqualCard;
import org.clafer.choco.constraint.propagator.PropContainsImpliesEqualCard2;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ContainsImpliesEqualTest {

    @Input(solutions = 64)
    public Object testContainsImpliesEqual(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     cond <- powerset [0,1,2]
         *     x <- powerset [0,1,2]
         *     y <- powerset [0,1,2]
         *     guard $ if 0 `elem` cond then x == y else null x
         *     return (cond, x, y)
         */
        CSetVar cond = cset("cond", env(0, 1, 2), solver);
        CSetVar x = cset("x", env(0, 1, 2), solver);
        CSetVar y = cset("y", env(0, 1, 2), solver);
        return $(cond, 0, x, y);
    }

    @Input(solutions = 12)
    public Object testXYDisjointCard(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     cond <- powerset [0,1,2]
         *     x <- powerset [0,1,2]
         *     guard $ length x == 0 || length x == 2
         *     y <- powerset [0,1,2]
         *     guard $ length y == 1
         *     guard $ if 0 `elem` cond then x == y else null x
         *     return (cond, x, y)
         */
        CSetVar cond = cset("cond", env(0, 1, 2), solver);
        CSetVar x = cset("x", env(0, 1, 2), ker(), card(0, 2), solver);
        CSetVar y = cset("y", env(0, 1, 2), ker(), card(1), solver);
        return $(cond, 0, x, y);
    }

    @Check
    public void check(TIntSet cond, int z, TIntSet x, TIntSet y) {
        if (cond.contains(z)) {
            assertEquals(x, y);
        } else {
            assertEquals(new TIntHashSet(), x);
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(CSetVar cond, int z, CSetVar x, CSetVar y) {
        return Constraints.containsImpliesEqualTest(cond.getSet(), z, x.getSet(), x.getCard(), y.getSet(), y.getCard());
    }
}
