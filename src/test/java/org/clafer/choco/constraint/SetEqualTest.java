package org.clafer.choco.constraint;

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
public class SetEqualTest {

    @Input(solutions = 8)
    public Object testSetEqual(Model model) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-1..2]
         *     s2 <- powerset [-2..1]
         *     guard $ s1 == s2
         *     return (s1, s2)
         */
        return $(model.setVar("s1", ker(), env(-1, 0, 1, 2)),
                model.setVar("s2", ker(), env(-2, -1, 0, 1)));
    }

    @Check
    public void check(int[] s1, int[] s2) {
        assertArrayEquals(s1, s2);
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(SetVar s1, SetVar s2) {
        return Constraints.equal(s1, s1.getCard(), s2, s2.getCard());
    }
}
