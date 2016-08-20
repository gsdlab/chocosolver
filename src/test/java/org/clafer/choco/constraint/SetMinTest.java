package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
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
public class SetMinTest {

    @Input(solutions = 37)
    public Object testSetMin(Model model) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     set <- powerset [-4..3]
         *     guard $ length set <= 2
         *     min <- [-4..4]
         *     guard $ if null set then min == 0 else || min == minimum set
         *     return (set, min)
         */
        SetVar set = model.setVar("set", ker(), env(-4, -3, -2, -1, 0, 1, 2, 3));
        IntVar card = model.intVar("|set|", 0, 2);
        set.setCard(card);
        IntVar min = model.intVar("min", -4, 4);
        return $(set, min, 0);
    }

    @Check
    public void check(int[] set, int min, int d) {
        if (set.length > 0) {
            assertEquals(set[0], min);
        } else {
            assertEquals(d, min);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(SetVar set, IntVar min, int d) {
        return Constraints.min(set, set.getCard(), min, d);
    }
}
