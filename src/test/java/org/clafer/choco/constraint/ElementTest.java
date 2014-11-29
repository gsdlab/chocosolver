package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import static solver.variables.VariableFactory.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ElementTest {

    @Input(solutions = 128)
    public Object testElement(Solver solver) {
        return $(enumerated("value", 0, 3, solver), enumeratedArray("array", 3, 0, 3, solver),
                enumerated("index", 0, 1, solver), 0);
    }

    @Check
    public void check(int value, int[] array, int index, int offset) {
        assertTrue(index + offset >= 0);
        assertTrue(index + offset < array.length);
        assertEquals(value, array[index + offset]);
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar value, @NonEmpty IntVar[] array, IntVar index, int offset) {
        return Constraints.element(value, array, index, offset);
    }
}
