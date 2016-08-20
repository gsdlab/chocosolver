package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class LengthTest {

    @Input(solutions = 85)
    public Object testLength(Model model) {
        /*
         * import Control.Monad
         *
         * solutions = [0..3] >>= flip replicateM [1..4]
         */
        return $(model.intVarArray("char", 3, 0, 4),
                model.intVar("length", 0, 3));
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
