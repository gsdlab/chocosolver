package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.chocosolver.solver.variables.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetEqualTest {

    @Input(solutions = 8)
    public Object testSetEqual(Solver solver) {
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
        return $(cset("s1", -1, 2, solver),
                cset("s2", -2, 1, solver));
    }

    @Check
    public void check(int[] s1, int[] s2) {
        assertArrayEquals(s1, s2);
    }

    @Test(timeout = 60000)
    public Constraint setup(CSetVar s1, CSetVar s2) {
        return Constraints.equal(s1.getSet(), s1.getCard(), s2.getSet(), s2.getCard());
    }
}
