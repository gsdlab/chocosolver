package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
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
public class SetUnionTest extends ConstraintTest<Triple<SetVar[], SetVar, Boolean>> {

    @Override
    protected void check(Triple<SetVar[], SetVar, Boolean> s) {
        TIntSet answer = new TIntHashSet();
        for (SetVar set : s.getFst()) {
            if (s.getThd()) {
                for (int c : set.getValue()) {
                    assertTrue(answer.add(c));
                }
            } else {
                answer.addAll(set.getValue());
            }
        }
        assertEquals(new TIntHashSet(s.getSnd().getValue()), answer);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<SetVar[], SetVar, Boolean>>() {
            @Override
            public Pair<Constraint, Triple<SetVar[], SetVar, Boolean>> setup(Solver solver) {
                CSetVar[] sets = toVars(randSets(nextInt(3) + 1), solver);
                CSetVar union = toVar(randSet(), solver);
                return pair(
                        Constraints.union(mapSet(sets), mapCard(sets), union.getSet(), union.getCard(), false),
                        triple(mapSet(sets), union.getSet(), false));
            }
        });
    }

    @Test(timeout = 60000)
    public void quickTestDisjoint() {
        randomizedTest(new TestCase<Triple<SetVar[], SetVar, Boolean>>() {
            @Override
            public Pair<Constraint, Triple<SetVar[], SetVar, Boolean>> setup(Solver solver) {
                CSetVar[] sets = toVars(randSets(nextInt(3) + 1), solver);
                CSetVar union = toVar(randSet(), solver);
                return pair(
                        Constraints.union(mapSet(sets), mapCard(sets), union.getSet(), union.getCard(), true),
                        triple(mapSet(sets), union.getSet(), true));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetUnion() {
        randomizedTest(new TestCase<Triple<SetVar[], SetVar, Boolean>>() {
            /*
             * import Control.Monad
             * import Data.List
             *
             * powerset = filterM (const [True, False])
             *
             * positive = do
             *     s1 <- powerset [-1..2]
             *     s2 <- powerset [-2..1]
             *     s3 <- powerset [-1..1]
             *     guard $ sort (nub $ s1 ++ s2) == sort s3
             *     return (s1, s2, s3)
             * 
             * negative = do
             *     s1 <- powerset [-1..2]
             *     s2 <- powerset [-2..1]
             *     s3 <- powerset [-1..1]
             *     guard $ sort (nub $ s1 ++ s2) /= sort s3
             *     return (s1, s2, s3)
             */
            @PositiveSolutions(64)
            @NegativeSolutions(1984)
            @Override
            public Pair<Constraint, Triple<SetVar[], SetVar, Boolean>> setup(Solver solver) {
                SetVar s1 = VF.set("s1", -1, 2, solver);
                IntVar s1Card = enforcedCardVar(s1);
                SetVar s2 = VF.set("s2", -2, 1, solver);
                IntVar s2Card = enforcedCardVar(s2);
                SetVar s3 = VF.set("s3", -1, 1, solver);
                IntVar s3Card = enforcedCardVar(s3);
                return pair(Constraints.union(new SetVar[]{s1, s2}, new IntVar[]{s1Card, s2Card}, s3, s3Card, false),
                        triple(new SetVar[]{s1, s2}, s3, false));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotSetUnionDisjoint() {
        randomizedTest(new TestCase<Triple<SetVar[], SetVar, Boolean>>() {
            /*
             * import Control.Monad
             * import Data.List
             *
             * powerset = filterM (const [True, False])
             *
             * positive = do
             *     s1 <- powerset [-1..2]
             *     s2 <- powerset [-2..1]
             *     s3 <- powerset [-1..1]
             *     guard $ (sort (nub $ s1 ++ s2) == sort s3) && (all (`notElem` s1) s2)
             *     return (s1, s2, s3)
             * 
             * negative = do
             *     s1 <- powerset [-1..2]
             *     s2 <- powerset [-2..1]
             *     s3 <- powerset [-1..1]
             *     guard $ (sort (nub $ s1 ++ s2) /= sort s3) || (any (`elem` s1) s2)
             *     return (s1, s2, s3)
             */
            @PositiveSolutions(27)
            @NegativeSolutions(2021)
            @Override
            public Pair<Constraint, Triple<SetVar[], SetVar, Boolean>> setup(Solver solver) {
                SetVar s1 = VF.set("s1", -1, 2, solver);
                IntVar s1Card = enforcedCardVar(s1);
                SetVar s2 = VF.set("s2", -2, 1, solver);
                IntVar s2Card = enforcedCardVar(s2);
                SetVar s3 = VF.set("s3", -1, 1, solver);
                IntVar s3Card = enforcedCardVar(s3);
                return pair(Constraints.union(new SetVar[]{s1, s2}, new IntVar[]{s1Card, s2Card}, s3, s3Card, true),
                        triple(new SetVar[]{s1, s2}, s3, true));
            }
        });
    }
}
