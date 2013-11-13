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
public class JoinFunctionTest extends ConstraintTest<Pair<Triple<SetVar, IntVar[], SetVar>, Integer>> {

    @Override
    protected void check(Pair<Triple<SetVar, IntVar[], SetVar>, Integer> s) {
        int[] take = s.getFst().getFst().getValue();
        int[] refs = getValues(s.getFst().getSnd());
        int[] to = s.getFst().getThd().getValue();
        int globalCardinality = s.getSnd();

        TIntIntHashMap count = new TIntIntHashMap();
        for (int i : take) {
            assertTrue(i >= 0 && i < refs.length);
            count.adjustOrPutValue(refs[i], 1, 1);
        }
        assertEquals(count.keySet(), new TIntHashSet(to));
        if (globalCardinality > 0 && count.size() > 0) {
            assertTrue(Util.max(count.valueCollection().iterator()) <= globalCardinality);
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, IntVar[], SetVar>, Integer>>() {
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, IntVar[], SetVar>, Integer>> setup(Solver solver) {
                CSetVar take = toCSetVar(randPositiveSet(), solver);
                IntVar[] refs = toIntVars(randInts(nextInt(3) + 1), solver);
                CSetVar to = toCSetVar(randSet(), solver);
                return pair(Constraints.joinFunction(take.getSet(), take.getCard(), refs, to.getSet(), to.getCard()),
                        pair(triple(take.getSet(), refs, to.getSet()), -1));
            }
        });
    }

    @Test(timeout = 60000)
    public void quickTestWithGlobalCardinality() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, IntVar[], SetVar>, Integer>>() {
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, IntVar[], SetVar>, Integer>> setup(Solver solver) {
                CSetVar take = toCSetVar(randPositiveSet(), solver);
                IntVar[] refs = toIntVars(randInts(nextInt(3) + 1), solver);
                CSetVar to = toCSetVar(randSet(), solver);
                int globalCardinality = nextIntBetween(1, 5);
                return pair(Constraints.joinFunction(take.getSet(), take.getCard(), refs, to.getSet(), to.getCard(), globalCardinality),
                        pair(triple(take.getSet(), refs, to.getSet()), globalCardinality));
            }
        });
    }

    @Test(timeout = 60000)
    public void testJoinFunction() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, IntVar[], SetVar>, Integer>>() {
            /*
             * positive = 2^3*4^3
             * 
             * negative = 2^3*4^3*2^4 - positive
             */
            @PositiveSolutions(512)
            @NegativeSolutions(7680)
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, IntVar[], SetVar>, Integer>> setup(Solver solver) {
                SetVar take = VF.set("take", 0, 2, solver);
                IntVar takeCard = enforcedCardVar(take);
                IntVar[] refs = new IntVar[3];
                for (int i = 0; i < refs.length; i++) {
                    refs[i] = VF.enumerated("ref" + i, 0, 3, solver);
                }
                SetVar to = VF.set("to", 0, 3, solver);
                IntVar toCard = enforcedCardVar(to);
                return pair(Constraints.joinFunction(take, takeCard, refs, to, toCard),
                        pair(triple(take, refs, to), -1));
            }
        });
    }

    @Test(timeout = 60000)
    public void testJoinFunctionWithGlobalUniqueness() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, IntVar[], SetVar>, Integer>>() {
            /*
             * import Control.Monad
             *
             * powerset = filterM (const [True, False])
             *
             * isUnique [] = True
             * isUnique (x : xs) = x `notElem` xs && isUnique xs
             *
             * positive = do
             *     from <- powerset [0..2]
             *     refs <- sequence $ replicate 3 [0..2]
             *     let to = map (refs !!) from
             *     guard $ isUnique to
             *     return (from, refs, to)
             * 
             * negative = 2^3*3^3*2^3 - length positive
             */
            @PositiveSolutions(168)
            @NegativeSolutions(1560)
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, IntVar[], SetVar>, Integer>> setup(Solver solver) {
                SetVar take = VF.set("take", 0, 2, solver);
                IntVar takeCard = enforcedCardVar(take);
                IntVar[] refs = new IntVar[3];
                for (int i = 0; i < refs.length; i++) {
                    refs[i] = VF.enumerated("ref" + i, 0, 2, solver);
                }
                SetVar to = VF.set("to", 0, 2, solver);
                IntVar toCard = enforcedCardVar(to);
                return pair(Constraints.joinFunction(take, takeCard, refs, to, toCard, 1),
                        pair(triple(take, refs, to), 1));
            }
        });
    }

    @Test(timeout = 60000)
    public void testJoinFunctionWithGlobalCardinality() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, IntVar[], SetVar>, Integer>>() {
            /*
             * import Control.Monad
             * import Data.List
             *
             * powerset = filterM (const [True, False])
             *
             * is2Unique = all ((<= 2) . length) . group . sort 
             *
             * positive = do
             *     from <- powerset [0..2]
             *     refs <- sequence $ replicate 3 [0..2]
             *     let to = map (refs !!) from
             *     guard $ is2Unique to
             *     return (from, refs, to)
             * 
             * negative = 2^3*3^3*2^3 - length positive
             */
            @PositiveSolutions(213)
            @NegativeSolutions(1515)
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, IntVar[], SetVar>, Integer>> setup(Solver solver) {
                SetVar take = VF.set("take", 0, 2, solver);
                IntVar takeCard = enforcedCardVar(take);
                IntVar[] refs = new IntVar[3];
                for (int i = 0; i < refs.length; i++) {
                    refs[i] = VF.enumerated("ref" + i, 0, 2, solver);
                }
                SetVar to = VF.set("to", 0, 2, solver);
                IntVar toCard = enforcedCardVar(to);
                return pair(Constraints.joinFunction(take, takeCard, refs, to, toCard, 2),
                        pair(triple(take, refs, to), 2));
            }
        });
    }

    @Test(timeout = 60000)
    public void testJoinFunctionFixedRefs() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, IntVar[], SetVar>, Integer>>() {
            @PositiveSolutions(4)
            @NegativeSolutions(12)
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, IntVar[], SetVar>, Integer>> setup(Solver solver) {
                SetVar take = VF.set("take", 0, 1, solver);
                IntVar takeCard = enforcedCardVar(take);
                IntVar[] refs = new IntVar[2];
                for (int i = 0; i < refs.length; i++) {
                    refs[i] = VF.fixed(5, solver);
                }
                SetVar to = VF.set("to", 4, 5, solver);
                IntVar toCard = enforcedCardVar(to);
                return pair(Constraints.joinFunction(take, takeCard, refs, to, toCard),
                        pair(triple(take, refs, to), -1));
            }
        });
    }
}
