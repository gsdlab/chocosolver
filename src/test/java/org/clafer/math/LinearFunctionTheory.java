package org.clafer.math;

import static org.junit.Assert.assertTrue;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(Theories.class)
public class LinearFunctionTheory {

    @DataPoints
    public static final LinearFunction[] function = new LinearFunction[]{
        new LinearFunction(0),
        new LinearFunction(1),
        new LinearFunction(-1),
        new LinearFunction(new int[]{1}, new Variable[]{new Variable("a", -1, 1)}, 0),
        new LinearFunction(new int[]{-1}, new Variable[]{new Variable("a", -1, 1)}, 0),
        new LinearFunction(new int[]{1, -1}, new Variable[]{new Variable("a", -1, 1), new Variable("b", -2, 2)}, 1)
    };

    @Theory
    public void lowHighBound(LinearFunction f) {
        assertTrue(f.getLowBound() <= f.getHighBound());
    }
}
