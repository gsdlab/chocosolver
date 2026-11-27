package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropContinuous;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ContinuousTest {

    @Input(solutions = 29)
    public Object testContinuous(Model model) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     set <- powerset [0..6]
         *     guard $ null set || maximum set - minimum set + 1 == length set
         *     return set
         */
        return $(model.setVar("set", ker(), env(0, 1, 2, 3, 4, 5, 6)));
    }

    @Check
    public void check(int[] set) {
        for (int i = 0; i < set.length - 1; i++) {
            assertEquals(set[i] + 1, set[i + 1]);
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(SetVar set) {
        return new Constraint("continuous", new PropContinuous(set, set.getCard()));
    }
}
