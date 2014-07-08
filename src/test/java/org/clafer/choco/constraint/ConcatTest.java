package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import solver.variables.CStringVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ConcatTest {

    @Input(solutions = 49)
    public Object testConcat(Solver solver) {
        /*
         * import Control.Monad
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b']
         * solutions = do
         *     left <- strings
         *     right <- strings
         *     concat <- strings
         *     guard $ left ++ right == cat
         *     return (left, right, cat)
         */
        return $(cstring("left", chars('a', 'b'), 3, solver),
                cstring("right", chars('a', 'b'), 3, solver),
                cstring("concat", chars('a', 'b'), 3, solver));
    }

    @Input(solutions = 15)
    public Object testEmptyLeft(Solver solver) {
        /*
         * import Control.Monad
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b']
         * solutions = do
         *     left <- [""]
         *     right <- strings
         *     concat <- strings
         *     guard $ left ++ right == cat
         *     return (left, right, cat)
         */
        return $(fixed("", solver),
                cstring("right", chars('a', 'b'), 3, solver),
                cstring("concat", chars('a', 'b'), 3, solver));
    }

    @Input(solutions = 15)
    public Object testEmptyRight(Solver solver) {
        /*
         * import Control.Monad
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b']
         * solutions = do
         *     left <- strings
         *     right <- [""]
         *     concat <- strings
         *     guard $ left ++ right == cat
         *     return (left, right, cat)
         */
        return $(cstring("left", chars('a', 'b'), 3, solver),
                fixed("", solver),
                cstring("concat", chars('a', 'b'), 3, solver));
    }

    @Input(solutions = 1)
    public Object testEmptyConcat(Solver solver) {
        /*
         * import Control.Monad
         *
         * strings = [0..3] >>= flip replicateM ['a', 'b']
         * solutions = do
         *     left <- strings
         *     right <- strings
         *     concat <- [""]
         *     guard $ left ++ right == cat
         *     return (left, right, cat)
         */
        return $(cstring("left", chars('a', 'b'), 3, solver),
                cstring("right", chars('a', 'b'), 3, solver),
                fixed("", solver));
    }

    @Check
    public void check(String left, String right, String concat) {
        assertEquals(left + right, concat);
    }

    @Test(timeout = 60000)
    public Constraint setup(CStringVar left, CStringVar right, CStringVar concat) {
        return Constraints.concat(
                left.getChars(), left.getLength(),
                right.getChars(), right.getLength(),
                concat.getChars(), concat.getLength());
    }
}
