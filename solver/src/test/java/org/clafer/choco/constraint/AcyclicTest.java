package org.clafer.choco.constraint;

import java.util.Set;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class AcyclicTest {

    @Input(solutions = 125)
    public Object testAcyclic(Model model) {
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
         */
        return $(model.intVarArray("edges", 4, 0, 4));
    }

    @Input(solutions = 0)
    public Object testTrivialCyclic(Model model) {
        return $(new IntVar[]{model.intVar(0)});
    }

    @Input(solutions = 1)
    public Object testTrivialAcyclic(Model model) {
        return $(new IntVar[]{model.intVar(1)});
    }

    @Check
    public void check(int[] edges) {
        KeyGraph<Integer> graph = new KeyGraph<>();
        for (int i = 0; i < edges.length; i++) {
            int from = i;
            int to = edges[from];
            assertNotEquals("Cycle of length 1", from, to);
            assertTrue(to >= 0 && to <= edges.length);
            graph.addEdge(from, to);
        }
        for (Set<Integer> component : GraphUtil.computeStronglyConnectedComponents(graph)) {
            assertTrue("Cycle of length > 1", component.size() == 1);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty IntVar[] edges) {
        return Constraints.acyclic(edges);
    }
}
