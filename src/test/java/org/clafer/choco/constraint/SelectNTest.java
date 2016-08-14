package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SelectNTest {

    @Input(solutions = 5)
    public Object testSelectN(Model model) {
        return $(model.boolVarArray("bool", 4),
                model.intVar("n", 0, 4));
    }

    @Check
    public void check(boolean[] bools, int n) {
        assertTrue(n >= 0);
        assertTrue(n <= bools.length);
        for (int i = 0; i < n; i++) {
            assertTrue(bools[i]);
        }
        for (int i = n; i < bools.length; i++) {
            assertFalse(bools[i]);
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint quickTest(BoolVar[] bools, IntVar n) {
        return Constraints.selectN(bools, n);
    }
}
