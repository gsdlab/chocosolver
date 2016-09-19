package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropElementArraySupport;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ElementArraySupportTest {

    @Input(solutions = 224)
    public Object testElementArraySupport(Model model) {
        return $(model.intVar("value", 0, 3), model.intVarArray("array", 3, 0, 3),
                model.intVar("index", -1, 0), 1, 0);
    }

    @Check
    public void check(int value, int[] array, int index, int offset, int support) {
        assertTrue(index + offset >= 0);
        assertTrue(index + offset < array.length);
        assertTrue(array[index + offset] == value || array[index + offset] == support);
    }

    @ArcConsistent(entailed = true)
    @Test(timeout = 60000)
    public Constraint setup(IntVar value, @NonEmpty IntVar[] array, IntVar index, int offset, int support) {
        return new Constraint("ElementArraySupport", new PropElementArraySupport(value, array, index, offset, support));
    }
}
