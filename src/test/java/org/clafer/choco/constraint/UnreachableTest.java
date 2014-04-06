package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class UnreachableTest extends ConstraintTest<Triple<IntVar[], Integer, Integer>> {

    @Override
    protected void check(Triple<IntVar[], Integer, Integer> s) {
        IntVar[] edges = s.getFst();
        int from = s.getSnd();
        int to = s.getThd();
        int cur = from;
        for (int i = 0; i < edges.length && cur < edges.length; i++) {
            assertTrue(cur >= 0);
            assertNotEquals(cur, to);
            cur = edges[cur].getValue();
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<IntVar[], Integer, Integer>>() {
            @Override
            public Pair<Constraint, Triple<IntVar[], Integer, Integer>> setup(Solver solver) {
                IntVar[] edges = toVars(randInts(nextInt(5) + 1), solver);
                int from = nextInt(edges.length);
                int to = nextInt(edges.length);
                return pair(Constraints.unreachable(edges, from, to),
                        triple(edges, from, to));
            }
        });
    }

    @Test(timeout = 60000)
    public void testUnreachable() {
        randomizedTest(new TestCase<Triple<IntVar[], Integer, Integer>>() {
            @PositiveSolutions(440)
            @NegativeSolutions(185)
            @Override
            public Pair<Constraint, Triple<IntVar[], Integer, Integer>> setup(Solver solver) {
                IntVar[] edges = VF.enumeratedArray("is", 4, 0, 4, solver);
                int from = 3;
                int to = 1;
                return pair(Constraints.unreachable(edges, from, to),
                        triple(edges, from, to));
            }
        });
    }
}
