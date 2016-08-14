package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.common.Util;

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

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar i, SetVar s) {
        return Constraints.singleton(i, s, s.getCard());
    }
}
