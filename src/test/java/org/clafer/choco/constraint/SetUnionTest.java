package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class SetUnionTest extends ConstraintTest {

    private void checkCorrectness(SetVar[] sets, SetVar union, boolean disjoint) {
        TIntSet answer = new TIntHashSet();
        for (SetVar set : sets) {
            if (disjoint) {
                for (int c : set.getValue()) {
                    assertTrue(Arrays.toString(sets), answer.add(c));
                }
            } else {
                answer.addAll(set.getValue());
            }
        }
        assertEquals(new TIntHashSet(union.getValue()), answer);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar[] sets = new SetVar[nextInt(3) + 1];
            for (int i = 0; i < sets.length; i++) {
                sets[i] = VF.set("s" + i, -nextInt(5), nextInt(5), solver);
            }
            IntVar[] setCards = enforcedCardVars(sets);
            SetVar union = VF.set("union", -nextInt(5), nextInt(5), solver);
            IntVar unionCard = enforcedCardVar(union);

            solver.post(Constraints.union(sets, setCards, union, unionCard, false));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(sets, union, false);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(sets, union, false);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTestDisjoint() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar[] sets = new SetVar[nextInt(3) + 1];
            for (int i = 0; i < sets.length; i++) {
                sets[i] = VF.set("s" + i, -nextInt(5), nextInt(5), solver);
            }
            IntVar[] setCards = enforcedCardVars(sets);
            SetVar union = VF.set("union", -nextInt(5), nextInt(5), solver);
            IntVar unionCard = enforcedCardVar(union);

            solver.post(Constraints.union(sets, setCards, union, unionCard, true));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(sets, union, true);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(sets, union, true);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSetUnion() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-4..2]
         *     s2 <- powerset [-2..4]
         *     s3 <- powerset [-2..2]
         *     guard $ sort (nub $ s1 ++ s2) == sort s3
         *     return (s1, s2, s3)
         */
        Solver solver = new Solver();

        SetVar s1 = VF.set("s1", -4, 2, solver);
        IntVar s1Card = enforcedCardVar(s1);
        SetVar s2 = VF.set("s2", -2, 4, solver);
        IntVar s2Card = enforcedCardVar(s2);
        SetVar s3 = VF.set("s3", -2, 2, solver);
        IntVar s3Card = enforcedCardVar(s3);

        solver.post(Constraints.union(new SetVar[]{s1, s2}, new IntVar[]{s1Card, s2Card},
                s3, s3Card, false));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(new SetVar[]{s1, s2}, s3, false);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(1024, count);
    }

    @Test(timeout = 60000)
    public void testSetUnionDisjoint() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-4..2]
         *     s2 <- powerset [-2..4]
         *     guard $ not $ any (`elem` s1) s2
         *     s3 <- powerset [-2..2]
         *     guard $ sort (nub $ s1 ++ s2) == sort s3
         *     return (s1, s2, s3)
         */
        Solver solver = new Solver();

        SetVar s1 = VF.set("s1", -4, 2, solver);
        IntVar s1Card = enforcedCardVar(s1);
        SetVar s2 = VF.set("s2", -2, 4, solver);
        IntVar s2Card = enforcedCardVar(s2);
        SetVar s3 = VF.set("s3", -2, 2, solver);
        IntVar s3Card = enforcedCardVar(s3);

        solver.post(Constraints.union(new SetVar[]{s1, s2}, new IntVar[]{s1Card, s2Card},
                s3, s3Card, true));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(new SetVar[]{s1, s2}, s3, true);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(243, count);
    }
}
