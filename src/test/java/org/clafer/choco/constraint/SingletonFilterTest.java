package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CSetVar;
import org.chocosolver.solver.variables.IntVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SingletonFilterTest {

    @Input(solutions = 5)
    public Object testSingletonFilter(Solver solver) {
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
        return $(enumerated("i", -3, 2, solver),
                cset("s", -2, 3, solver),
                2);
    }

    @Check
    public void check(int i, int[] s, int filter) {
        assertArrayEquals(i == filter ? new int[]{} : new int[]{i}, s);
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar i, CSetVar s, int filter) {
        return Constraints.singletonFilter(i, s.getSet(), s.getCard(), filter);
    }
}
