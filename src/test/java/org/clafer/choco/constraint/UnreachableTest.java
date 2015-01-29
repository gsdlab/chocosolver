package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assert.*;
import static org.junit.Assume.*;
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
public class UnreachableTest {

    @Input(solutions = 440)
    public Object testUnreachable(Solver solver) {
        return $(enumeratedArray("edge", 4, 0, 4, solver), 3, 1);
    }

    @Check
    public void check(int[] edges, int from, int to) {
        int cur = from;
        for (int i = 0; i < edges.length && cur < edges.length; i++) {
            assertTrue(cur >= 0);
            assertNotEquals(cur, to);
            cur = edges[cur];
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty IntVar[] edges, @Positive int from, @Positive int to) {
        assumeTrue(from < edges.length);
        assumeTrue(to < edges.length);
        return Constraints.unreachable(edges, from, to);
    }
}
