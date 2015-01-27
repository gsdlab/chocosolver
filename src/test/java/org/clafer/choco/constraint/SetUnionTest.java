package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.chocosolver.solver.variables.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetUnionTest {

    @Input(solutions = 64)
    public Object testSetUnion(Solver solver) {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-1..2]
         *     s2 <- powerset [-2..1]
         *     s3 <- powerset [-1..1]
         *     guard $ sort (nub $ s1 ++ s2) == sort s3
         *     return (s1, s2, s3)
         */
        return $(
                new CSetVar[]{
                    cset("s1", -1, 2, solver),
                    cset("s2", -2, 1, solver)},
                cset("s3", -1, 1, solver), false);
    }

    @Input(solutions = 27)
    public Object testSetUnionDisjoint(Solver solver) {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-1..2]
         *     s2 <- powerset [-2..1]
         *     s3 <- powerset [-1..1]
         *     guard $ (sort (nub $ s1 ++ s2) == sort s3) && (all (`notElem` s1) s2)
         *     return (s1, s2, s3)
         */
        return $(
                new CSetVar[]{
                    cset("s1", -1, 2, solver),
                    cset("s2", -2, 1, solver)},
                cset("s3", -1, 1, solver), true);
    }

    @Check
    public void check(TIntSet[] sets, TIntSet union, boolean disjoint) {
        TIntSet answer = new TIntHashSet();
        for (TIntSet set : sets) {
            if (disjoint) {
                for (int c : set.toArray()) {
                    assertTrue(answer.add(c));
                }
            } else {
                answer.addAll(set);
            }
        }
        assertEquals(union, answer);
    }

    @Test(timeout = 60000)
    public Constraint setup(CSetVar[] sets, CSetVar union, boolean disjoint) {
        return Constraints.union(
                mapSet(sets), mapCard(sets),
                union.getSet(), union.getCard(),
                disjoint);
    }
}
