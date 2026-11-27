package org.clafer.choco.constraint;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ArrayEqualTest {

    @Check
    public void check(int[] a1, int[] a2) {
        assertArrayEquals(a1, a2);
    }

    @ArcConsistent(entailed = true)
    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty IntVar[] a1, IntVar[] a2) {
        assumeTrue(a1.length == a2.length);
        return Constraints.equal(a1, a2);
    }
}
