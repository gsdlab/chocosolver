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
import static org.junit.Assert.assertArrayEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SingletonFilterTest {

    @Input(solutions = 5)
    public Object testSingletonFilter(Model model) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     i <- [-3..2]
         *     s <- powerset [-2..3]
         *     guard $ [i] == s
         *     return (i, s)
         */
        return $(model.intVar("i", -3, 2),
                model.setVar("s", ker(), env(-2, -1, 0, 1, 2, 3)),
                2);
    }

    @Check
    public void check(int i, int[] s, int filter) {
        assertArrayEquals(i == filter ? new int[]{} : new int[]{i}, s);
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar i, SetVar s, int filter) {
        return Constraints.singletonFilter(i, s, s.getCard(), filter);
    }
}
