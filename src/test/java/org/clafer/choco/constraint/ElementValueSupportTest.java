package org.clafer.choco.constraint;

import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.Sample;
import org.clafer.choco.constraint.propagator.PropElementValueSupport;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ElementValueSupportTest {

    @Check
    public void check(int value, int[] array, int index, int offset, int support) {
        assertTrue(index + offset >= 0);
        assertTrue(index + offset < array.length);
        assertTrue(array[index + offset] == value || value == support);
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar value, @NonEmpty IntVar[] array, IntVar index, int offset, int support) {
        return new Constraint("ElementValueSupport", new PropElementValueSupport(value, array, index, offset, support));
    }
}
