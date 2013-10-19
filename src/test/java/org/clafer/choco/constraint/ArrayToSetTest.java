package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.clafer.common.Util;
import org.clafer.choco.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Cause;
import solver.Solver;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class ArrayToSetTest extends ConstraintTest {

    private void checkCorrectness(IntVar[] array, SetVar set) {
        int[] $array = PropUtil.getValues(array);
        int[] $set = set.getValue();

        assertEquals(new TIntHashSet($array), new TIntHashSet($set));
    }

    private void checkCorrectness(IntVar[] array, SetVar set, int globalCardinality) {
        int[] $array = PropUtil.getValues(array);
        int[] $set = set.getValue();

        assertEquals(new TIntHashSet($array), new TIntHashSet($set));
        for (int i = 0; i < $array.length; i++) {
            int count = 0;
            for (int j = i; j < $array.length; j++) {
                if ($array[i] == $array[j]) {
                    count++;
                }
            }
            assertTrue(Arrays.toString($array), count <= globalCardinality);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int low = nextInt(5) + 1;
            int high = nextInt(5);

            IntVar[] array = VF.enumeratedArray("array", low + high, -low - nextInt(10), high + nextInt(10), solver);
            SetVar set = VF.set("set", Util.range(-low - nextInt(10), high + nextInt(10)), solver);
            IntVar setCard = enforcedCardVar(set);

            solver.post(Constraints.arrayToSet(array, set, setCard));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(array, set);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(array, set);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTestGlobalCardinality() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int low = nextInt(5) + 1;
            int high = nextInt(5);
            int globalCardinality = nextInt(5) + 1;

            IntVar[] array = VF.enumeratedArray("array", low + high, -low - nextInt(10), high + nextInt(10), solver);
            SetVar set = VF.set("set", Util.range(-low - nextInt(10), high + nextInt(10)), solver);
            IntVar setCard = enforcedCardVar(set);

            solver.post(Constraints.arrayToSet(array, set, setCard, globalCardinality));

            assertTrue(solver.toString(), randomizeStrategy(solver).findSolution());
            checkCorrectness(array, set, globalCardinality);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(array, set, globalCardinality);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTestLargeDomain() {
        Solver solver = new Solver();

        IntVar[] array = VF.enumeratedArray("array", 5, 0, 10000, solver);
        SetVar set = VF.set("set", Util.range(0, 10000), solver);
        IntVar setCard = enforcedCardVar(set);

        solver.post(Constraints.arrayToSet(array, set, setCard));

        assertTrue(randomizeStrategy(solver).findSolution());
        checkCorrectness(array, set);
    }

    @Test(timeout = 60000)
    public void testArrayToSet() {
        /*
         * length $ sequence $ replicate 3 [0..5]
         */
        Solver solver = new Solver();

        IntVar[] ivars = VF.enumeratedArray("ivar", 3, 0, 5, solver);
        SetVar svar = VF.set("svar", new int[]{0, 1, 2, 3, 4, 5}, solver);
        IntVar svarCard = enforcedCardVar(svar);

        solver.post(Constraints.arrayToSet(ivars, svar, svarCard));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(ivars, svar);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(216, count);
    }

    @Test(timeout = 60000)
    public void testArrayToSetWithGlobalCardinality() {
        /*
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         * 
         * length $ filter isUnique $ sequence $ replicate 3 [0..5]
         */
        Solver solver = new Solver();

        IntVar[] ivars = VF.enumeratedArray("ivar", 3, 0, 5, solver);
        SetVar svar = VF.set("svar", new int[]{0, 1, 2, 3, 4, 5}, solver);
        IntVar svarCard = enforcedCardVar(svar, 0, 4);
        int globalCardinality = 1;

        solver.post(Constraints.arrayToSet(ivars, svar, svarCard, globalCardinality));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(ivars, svar, globalCardinality);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(120, count);
    }

    @Test(timeout = 60000)
    public void testArrayToSetWithGlobalCardinalityPartiallySolved() throws ContradictionException {
        /*
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         * 
         * length $ filter isUnique $ sequence $ replicate 3 [0..5]
         */
        Solver solver = new Solver();

        IntVar[] ivars = new IntVar[3];
        ivars[0] = VF.enumerated("ivar0", new int[]{0, 2}, solver);
        ivars[1] = VF.enumerated("ivar1", new int[]{1, 2}, solver);
        ivars[2] = VF.enumerated("ivar2", new int[]{0, 1, 2, 3}, solver);
        SetVar svar = VF.set("svar", new int[]{0, 1, 2, 3}, solver);
        IntVar svarCard = enforcedCardVar(svar);
        int globalCardinality = 1;

        solver.post(Constraints.arrayToSet(ivars, svar, svarCard, globalCardinality));

        randomizeStrategy(solver).propagate();
        svar.removeFromEnvelope(globalCardinality, Cause.Null);
        solver.propagate();
    }
}
