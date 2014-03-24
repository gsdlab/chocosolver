package org.clafer.choco.constraint;

import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
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
public class AcyclicTest extends ConstraintTest<IntVar[]> {

    @Override
    protected void check(IntVar[] s) {
        KeyGraph<Integer> graph = new KeyGraph<>();
        for (int i = 0; i < s.length; i++) {
            int from = i;
            int to = s[from].getValue();
            assertNotEquals("Cycle of length 1", from, to);
            assertTrue(to >= 0 && to <= s.length);
            graph.addEdge(from, to);
        }
        for (Set<Integer> component : GraphUtil.computeStronglyConnectedComponents(graph)) {
            assertTrue("Cycle of length > 1", component.size() == 1);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<IntVar[]>() {
            @Override
            public Pair<Constraint, IntVar[]> setup(Solver solver) {
                IntVar[] edges = toVars(randInts(nextInt(5) + 1), solver);
                return pair(Constraints.acyclic(edges), edges);
            }
        });
    }

    @Test(timeout = 60000)
    public void testAcyclic() {
        /*
         * import Control.Monad
         * import Data.Graph
         *
         * positive = do
         *     is <- replicateM 4 [0..4]
         *     guard $ all isAcyclic $ stronglyConnComp [(from, from, [to]) | (from, to) <- zip [0..] is]
         *     return is
         *     where
         *         isAcyclic AcyclicSCC{} = True
         *         isAcyclic _ = False
         *        
         * negative = 5^4 - length positive
         */
        randomizedTest(new TestCase<IntVar[]>() {
            @PositiveSolutions(125)
            @NegativeSolutions(500)
            @Override
            public Pair<Constraint, IntVar[]> setup(Solver solver) {
                IntVar[] edges = VF.enumeratedArray("edges", 4, 0, 4, solver);
                return pair(Constraints.acyclic(edges), edges);
            }
        });
    }
}
