package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.dom;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
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
    public Object testContainsImpliesEqual(Model model) {
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
        SetVar cond = model.setVar("cond", ker(), env(0, 1, 2));
        SetVar x = model.setVar("x", ker(), env(0, 1, 2));
        SetVar y = model.setVar("y", ker(), env(0, 1, 2));
        return $(cond, 0, x, y);
    }

    @Input(solutions = 12)
    public Object testXYDisjointCard(Model model) {
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
        SetVar cond = model.setVar("cond", ker(), env(0, 1, 2));
        SetVar x = model.setVar("x", ker(), env(0, 1, 2));
        IntVar xCard = model.intVar("|x|", dom(0, 2));
        x.setCard(xCard);
        SetVar y = model.setVar("y", ker(), env(0, 1, 2));
        y.setCard(model.intVar(1));
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
    public Constraint setup(SetVar cond, int z, SetVar x, SetVar y) {
        return Constraints.containsImpliesEqualTest(cond, z, x, x.getCard(), y, y.getCard());
    }
}
