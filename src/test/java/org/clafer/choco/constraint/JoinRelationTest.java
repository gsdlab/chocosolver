package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.choco.constraint.propagator.PropUtil;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class JoinRelationTest extends ConstraintTest {

    private void checkCorrectness(SetVar take, SetVar[] children, SetVar to) {
        int[] $take = take.getValue();
        int[][] $children = PropUtil.getValues(children);
        int[] $to = to.getValue();

        TIntHashSet set = new TIntHashSet();

        for (int t : $take) {
            for (int c : $children[t]) {
                assertTrue(Util.in(c, $to));
                set.add(c);
            }
        }
        assertEquals(set.size(), $to.length);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int num = nextInt(5);

            SetVar take = VF.set("take", Util.fromTo(0, num), solver);
            IntVar takeCard = enforcedCardVar(take);
            SetVar[] children = new SetVar[num];
            for (int i = 0; i < children.length; i++) {
                children[i] = VF.set("child" + i, Util.range(0, nextInt(10)), solver);
            }
            IntVar[] childrenCards = enforcedCardVars(children);
            SetVar to = VF.set("to", Util.range(0, nextInt(10)), solver);
            IntVar toCard = enforcedCardVar(to);

            solver.post(Constraints.joinInjectiveRelation(take, takeCard, children, childrenCards, to, toCard));
            if (num > 1) {
                solver.post(SCF.all_disjoint(children));
            }

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(take, children, to);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(take, children, to);
            }
        }
    }

    @Test(timeout = 60000)
    public void testJoinRelation() {
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
         *     child0 <- powerset [0..4]
         *     child1 <- powerset [0..4]
         *     child2 <- powerset [0..4]
         *     guard $ child0 `disjoint` child1 && child0 `disjoint` child2 && child1 `disjoint` child2
         *     return (take, child0, child1, child2)
         */
        Solver solver = new Solver();

        SetVar take = VF.set("take", new int[]{0, 1, 2}, solver);
        IntVar takeCard = enforcedCardVar(take);
        SetVar[] children = new SetVar[3];
        for (int i = 0; i < children.length; i++) {
            children[i] = VF.set("child" + i, new int[]{0, 1, 2, 3, 4}, solver);
        }
        IntVar[] childrenCards = enforcedCardVars(children);
        SetVar to = VF.set("to", new int[]{0, 1, 2, 3, 4}, solver);
        IntVar toCard = enforcedCardVar(to);

        solver.post(Constraints.joinInjectiveRelation(take, takeCard, children, childrenCards, to, toCard));
        solver.post(SCF.all_disjoint(children));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(take, children, to);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(8192, count);
    }

    @Test(timeout = 60000)
    public void testJoinNonDisjoint() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     take   <- powerset [0..2]
         *     child0 <- powerset [0..2]
         *     child1 <- powerset [0..2]
         *     child2 <- powerset [0..2]
         *     return (take, child0, child1, child2)
         */
        Solver solver = new Solver();

        SetVar take = VF.set("take", new int[]{0, 1, 2}, solver);
        SetVar[] children = new SetVar[3];
        for (int i = 0; i < children.length; i++) {
            children[i] = VF.set("child" + i, new int[]{0, 1, 2}, solver);
        }
        SetVar to = VF.set("to", new int[]{0, 1, 2}, solver);

        solver.post(Constraints.joinRelation(take, children, to));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(take, children, to);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(4096, count);
    }
}
