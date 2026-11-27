package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import static org.junit.Assert.assertArrayEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SingletonTest {

    @Input(solutions = 5)
    public Object testSingleton(Model model) {
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
                model.setVar("s", new int[0], Util.range(-2, 3)));
    }

    @Check
    public void check(int i, int[] s) {
        assertArrayEquals(new int[]{i}, s);
    }

    @ArcConsistent(entailed = true)
    @Test(timeout = 60000)
    public Constraint setup(IntVar i, SetVar s) {
        return Constraints.singleton(i, s, s.getCard());
    }
}
