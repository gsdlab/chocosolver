package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.Positive;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SubarrayTest {

    @Input(solutions = 513)
    public Object testSubstring(Model model) {
        return $(model.intVarArray("substring", 2, -1, 1),
                model.intVar("substringLength", 0, 2),
                model.intVar("index", 0, 2),
                model.intVarArray("supstring", 4, -1, 1));
    }

    @Check
    public void check(int[] subarray, int sublength, int index, int[] array) {
        assertTrue(sublength >= 0);
        assertTrue(index >= 0);
        assertTrue(index < array.length);
        for (int i = 0; i < sublength; i++) {
            assertTrue(index + i < array.length);
            assertTrue(i < subarray.length);
            assertNotEquals(-1, subarray[i]);
            assertEquals(array[index + i], subarray[i]);
        }
        for (int i = sublength; i < subarray.length; i++) {
            assertEquals(-1, subarray[i]);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(IntVar[] subarray, IntVar sublength, @Positive IntVar index, IntVar[] array) {
        return Constraints.subarray(subarray, sublength, index, array);
    }
}
