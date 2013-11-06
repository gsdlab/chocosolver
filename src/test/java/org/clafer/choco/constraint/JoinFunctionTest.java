package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.choco.constraint.propagator.PropUtil;
import org.clafer.common.Util;
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
public class JoinFunctionTest extends ConstraintTest {

    private void checkCorrectness(SetVar take, IntVar[] refs, SetVar to) {
        int[] $take = take.getValue();
        int[] $refs = PropUtil.getValues(refs);
        int[] $to = to.getValue();

        TIntHashSet set = new TIntHashSet();

        for (int f : $take) {
            assertTrue(Util.in($refs[f], $to));
            set.add($refs[f]);
        }
        assertEquals(set.size(), $to.length);
    }

    @Test()
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int num = nextInt(10);

            SetVar take = VF.set("take", Util.fromTo(0, num), solver);
            IntVar takeCard = enforcedCardVar(take);
            IntVar[] refs = new IntVar[num];
            for (int i = 0; i < refs.length; i++) {
                refs[i] = VF.enumerated("ref" + i, 0, nextInt(10), solver);
            }
            SetVar to = VF.set("to", Util.fromTo(0, nextInt(10)), solver);
            IntVar toCard = enforcedCardVar(to);

            solver.post(Constraints.joinFunction(take, takeCard, refs, to, toCard));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(take, refs, to);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(take, refs, to);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTestSingleValue() {
        Solver solver = new Solver();

        SetVar take = VF.set("take", Util.fromTo(0, 2), solver);
        IntVar takeCard = enforcedCardVar(take);
        IntVar[] refs = new IntVar[2];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VF.enumerated("ref" + i, 5, 5, solver);
        }
        SetVar to = VF.set("to", Util.range(0, 5), solver);
        IntVar toCard = enforcedCardVar(to);

        solver.post(Constraints.joinFunction(take, takeCard, refs, to, toCard));

        assertTrue(randomizeStrategy(solver).findSolution());
        checkCorrectness(take, refs, to);
        assertTrue(solver.nextSolution());
        checkCorrectness(take, refs, to);
        assertTrue(solver.nextSolution());
        checkCorrectness(take, refs, to);
        assertTrue(solver.nextSolution());
        checkCorrectness(take, refs, to);
        assertFalse(solver.nextSolution());
    }

    @Test(timeout = 60000)
    public void testJoinFunction() {
        Solver solver = new Solver();

        SetVar take = VF.set("take", new int[]{0, 1, 2}, solver);
        IntVar takeCard = enforcedCardVar(take);
        IntVar[] refs = new IntVar[3];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VF.enumerated("ref" + i, 0, 4, solver);
        }
        SetVar to = VF.set("to", new int[]{0, 1, 2, 3, 4}, solver);
        IntVar toCard = enforcedCardVar(to);

        solver.post(Constraints.joinFunction(take, takeCard, refs, to, toCard));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(take, refs, to);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(1000, count);
    }

    @Test(timeout = 60000)
    public void testJoinFunctionWithGlobalUniqueness() {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * solutions = do
         *     from <- powerset [0..2]
         *     refs <- sequence $ replicate 3 [0..4]
         *     let to = map (refs !!) from
         *     guard $ isUnique to
         *     return (from, refs, to)
         */
        Solver solver = new Solver();

        SetVar take = VF.set("take", new int[]{0, 1, 2}, solver);
        IntVar takeCard = enforcedCardVar(take);
        IntVar[] refs = new IntVar[3];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VF.enumerated("ref" + i, 0, 4, solver);
        }
        SetVar to = VF.set("to", new int[]{0, 1, 2, 3, 4}, solver);
        IntVar toCard = enforcedCardVar(to);

        solver.post(Constraints.joinFunction(take, takeCard, refs, to, toCard, 1));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(take, refs, to);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(860, count);
    }

    @Test(timeout = 60000)
    public void testJoinFunctionWithGlobalCardinality() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * is2Unique = all ((<= 2) . length) . group . sort 
         *
         * solutions = do
         *     from <- powerset [0..2]
         *     refs <- sequence $ replicate 3 [0..4]
         *     let to = map (refs !!) from
         *     guard $ is2Unique to
         *     return (from, refs, to)
         */
        Solver solver = new Solver();

        SetVar take = VF.set("take", new int[]{0, 1, 2}, solver);
        IntVar takeCard = enforcedCardVar(take);
        IntVar[] refs = new IntVar[3];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VF.enumerated("ref" + i, 0, 4, solver);
        }
        SetVar to = VF.set("to", new int[]{0, 1, 2, 3, 4}, solver);
        IntVar toCard = enforcedCardVar(to);

        solver.post(Constraints.joinFunction(take, takeCard, refs, to, toCard, 2));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(take, refs, to);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(995, count);
    }
}
