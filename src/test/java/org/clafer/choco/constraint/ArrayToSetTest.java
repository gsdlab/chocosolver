package org.clafer.choco.constraint;

import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class ArrayToSetTest extends ConstraintTest<Triple<IntVar[], SetVar, Integer>> {

    @Override
    protected void check(Triple<IntVar[], SetVar, Integer> s) {
        int[] array = getValues(s.getFst());
        int[] set = s.getSnd().getValue();
        int globalCardinality = s.getThd();

        TIntIntHashMap count = new TIntIntHashMap();
        for (int i = 0; i < array.length; i++) {
            count.adjustOrPutValue(array[i], 1, 1);
        }
        assertEquals(count.keySet(), new TIntHashSet(set));
        if (globalCardinality > 0 && count.size() > 0) {
            assertTrue(Util.max(count.valueCollection().iterator()) <= globalCardinality);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<IntVar[], SetVar, Integer>>() {
            @Override
            public Pair<Constraint, Triple<IntVar[], SetVar, Integer>> setup(Solver solver) {
                IntVar[] array = toVars(randInts(nextInt(3) + 1), solver);
                CSetVar set = toVar(randSet(), solver);
                return pair(Constraints.arrayToSet(array, set.getSet(), set.getCard()),
                        triple(array, set.getSet(), -1));
            }
        });
    }

    @Test(timeout = 60000)
    public void quickTestWithGlobalCardinality() {
        randomizedTest(new TestCase<Triple<IntVar[], SetVar, Integer>>() {
            @Override
            public Pair<Constraint, Triple<IntVar[], SetVar, Integer>> setup(Solver solver) {
                IntVar[] array = toVars(randInts(nextInt(3) + 1), solver);
                CSetVar set = toVar(randSet(), solver);
                int globalCardinality = nextIntBetween(1, 5);
                return pair(Constraints.arrayToSet(array, set.getSet(), set.getCard(), globalCardinality),
                        triple(array, set.getSet(), globalCardinality));
            }
        });
    }

    @Test(timeout = 60000)
    public void quickTestLargeDomain() {
        randomizedTest(new TestCase<Triple<IntVar[], SetVar, Integer>>() {
            @Override
            public Pair<Constraint, Triple<IntVar[], SetVar, Integer>> setup(Solver solver) {
                IntVar[] array = VF.enumeratedArray("array", 5, 0, 10000, solver);
                SetVar set = VF.set("set", 0, 10000, solver);
                IntVar setCard = enforcedCardVar(set, 0, 5);
                return pair(Constraints.arrayToSet(array, set, setCard),
                        triple(array, set, -1));
            }
        });
    }

    @Test(timeout = 60000)
    public void testArrayToSet() {
        randomizedTest(new TestCase<Triple<IntVar[], SetVar, Integer>>() {
            /*
             * positive = sequence $ replicate 3 [0..5]
             * 
             * negative = 6^3*2^6 - length positive
             */
            @PositiveSolutions(216)
            @NegativeSolutions(13608)
            @Override
            public Pair<Constraint, Triple<IntVar[], SetVar, Integer>> setup(Solver solver) {
                IntVar[] array = VF.enumeratedArray("array", 3, 0, 5, solver);
                SetVar set = VF.set("set", 0, 5, solver);
                IntVar setcard = enforcedCardVar(set);
                return pair(Constraints.arrayToSet(array, set, setcard),
                        triple(array, set, -1));
            }
        });
    }

    @Test(timeout = 60000)
    public void testArrayToSetWithGlobalCardinality() {
        randomizedTest(new TestCase<Triple<IntVar[], SetVar, Integer>>() {
            /*
             * isUnique [] = True
             * isUnique (x : xs) = x `notElem` xs && isUnique xs
             * 
             * positive =  filter isUnique $ sequence $ replicate 3 [0..5]
             * 
             * negative = 6^3*(2^6-6-1) - length positive
             */
            @PositiveSolutions(120)
            @NegativeSolutions(12192)
            @Override
            public Pair<Constraint, Triple<IntVar[], SetVar, Integer>> setup(Solver solver) {
                IntVar[] arrays = VF.enumeratedArray("array", 3, 0, 5, solver);
                SetVar set = VF.set("set", 0, 5, solver);
                IntVar setCard = enforcedCardVar(set, 0, 4);
                int globalCardinality = 1;
                return pair(Constraints.arrayToSet(arrays, set, setCard, globalCardinality),
                        triple(arrays, set, globalCardinality));
            }
        });
    }
}
