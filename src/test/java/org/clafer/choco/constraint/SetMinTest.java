package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CSetVar;
import org.chocosolver.solver.variables.IntVar;
import static org.chocosolver.solver.variables.Var.card;
import static org.chocosolver.solver.variables.Var.cset;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.chocosolver.solver.variables.VariableFactory.enumerated;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetMinTest {

    @Input(solutions = 37)
    public Object testSetMin(Solver solver) {
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
        return $(cset("set", env(-4, -3, -2, -1, 0, 1, 2, 3), ker(), card(0, 1, 2), solver),
                enumerated("min", -4, 4, solver), 0);
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
    public Constraint setup(CSetVar set, IntVar min, int d) {
        return Constraints.min(set.getSet(), set.getCard(), min, d);
    }
}
