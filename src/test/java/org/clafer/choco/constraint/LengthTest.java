package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class LengthTest {

    @Input(solutions = 85)
    public Object testLength(Solver solver) {
        /*
         * import Control.Monad
         *
         * solutions = [0..3] >>= flip replicateM [1..4]
         */
        return $(enumeratedArray("char", 3, 0, 4, solver),
                enumerated("length", 0, 3, solver));
    }

    @Check
    public void check(int[] chars, int length) {
        assertTrue(length >= 0);
        assertTrue(length <= chars.length);
        for (int i = 0; i < length; i++) {
            assertNotEquals(0, chars[i]);
        }
        for (int i = length; i < chars.length; i++) {
            assertEquals(0, chars[i]);
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar[] chars, IntVar length) {
        return Constraints.length(chars, length);
    }
}
