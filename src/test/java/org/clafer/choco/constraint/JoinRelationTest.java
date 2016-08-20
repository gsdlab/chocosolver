package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.chocosolver.solver.variables.Var.mapCard;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.Positive;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class JoinRelationTest {

    @Input(solutions = 576)
    public Object testJoinRelation(Model model) {
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
        return $(model.setVar("take", ker(), env(0, 1, 2)),
                new SetVar[]{
                    model.setVar("c1", ker(), env(-1, 0, 1)),
                    model.setVar("c2", ker(), env(0, 1)),
                    model.setVar("c3", ker(), env(-1, 0)),},
                model.setVar("to", ker(), env(0, 1)),
                false);
    }

    @Input(solutions = 504)
    public Object testJoinInjectiveRelation(Model model) {
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
        return $(model.setVar("take", ker(), env(0, 1, 2)),
                new SetVar[]{
                    model.setVar("c1", ker(), env(-1, 0, 1)),
                    model.setVar("c2", ker(), env(0, 1)),
                    model.setVar("c3", ker(), env(-1, 0)),},
                model.setVar("to", ker(), env(0, 1)),
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
    public Constraint setup(@Positive SetVar take, SetVar[] children, SetVar to, boolean injective) {
        return injective
                ? Constraints.joinInjectiveRelation(
                        take, take.getCard(), children, mapCard(children),
                        to, to.getCard())
                : Constraints.joinRelation(take, children, to);
    }
}
