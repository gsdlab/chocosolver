package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.test.Positive;
import org.clafer.test.TestUtil.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class JoinRelationTest {

    @Input(solutions = 576)
    public Object testJoinRelation(Solver solver) {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * disjoint [] ys = True
         * disjoint (x:xs) ys = x `notElem` ys && disjoint xs ys
         *
         * solutions = do
         *     take   <- powerset [0..2]
         *     child0 <- powerset [-1..1]
         *     child1 <- powerset [0..1]
         *     child2 <- powerset [-1..0]
         *     to <- powerset [0..1]
         *     guard $ to == sort (nub $ concat [[child0, child1, child2] !! i | i <- take])
         *     return (take, child0, child1, child2)
         */
        return $(
                cset("take", 0, 2, solver),
                new CSetVar[]{
                    cset("c1", -1, 1, solver),
                    cset("c2", 0, 1, solver),
                    cset("c3", -1, 0, solver)
                },
                cset("to", 0, 1, solver),
                false);
    }

    @Input(solutions = 504)
    public Object testJoinInjectiveRelation(Solver solver) {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     take   <- powerset [0..2]
         *     child0 <- powerset [-1..1]
         *     child1 <- powerset [0..1]
         *     child2 <- powerset [-1..0]
         *     let takeSet = concat [[child0, child1, child2] !! i | i <- take]
         *     to <- powerset [0..1]
         *     guard $ to == sort takeSet
         *     return (take, child0, child1, child2)
         */
        return $(cset("take", 0, 2, solver),
                new CSetVar[]{
                    cset("c1", -1, 1, solver),
                    cset("c2", 0, 1, solver),
                    cset("c3", -1, 0, solver)
                }, cset("to", 0, 1, solver),
                true);
    }

    @Check
    public void check(int[] take, int[][] children, TIntSet to, boolean injective) {
        TIntHashSet set = new TIntHashSet();

        for (int t : take) {
            assertTrue(t >= 0 && t < children.length);
            for (int c : children[t]) {
                boolean unique = set.add(c);
                if (injective) {
                    assertTrue(unique);
                }
            }
        }
        assertEquals(set, to);
    }

    @Test(timeout = 60000)
    public Constraint setup(@Positive CSetVar take, CSetVar[] children, CSetVar to, boolean injective) {
        return injective
                ? Constraints.joinInjectiveRelation(
                        take.getSet(), take.getCard(), mapSet(children), mapCard(children),
                        to.getSet(), to.getCard())
                : Constraints.joinRelation(take.getSet(), mapSet(children), to.getSet());
    }
}
