package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.LCF;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class JoinRelationTest extends ConstraintTest<Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>> {

    @Override
    protected void check(Pair<Triple<SetVar, SetVar[], SetVar>, Boolean> s) {
        TIntHashSet set = new TIntHashSet();

        for (int t : s.getFst().getFst().getValue()) {
            assertTrue(t >= 0 && t < s.getFst().getSnd().length);
            for (int c : s.getFst().getSnd()[t].getValue()) {
                assertTrue(Util.in(c, s.getFst().getThd().getValue()));
                set.add(c);
            }
        }
        assertEquals(set.size(), s.getFst().getThd().getEnvelopeSize());
        if (s.getSnd()) {
            int sum = 0;
            TIntHashSet disjointSet = new TIntHashSet();
            for (SetVar child : s.getFst().getSnd()) {
                sum += child.getEnvelopeSize();
                disjointSet.addAll(child.getValue());
            }
            assertEquals(sum, disjointSet.size());
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>>() {
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>> setup(Solver solver) {
                CSetVar take = toCSetVar(randPositiveSet(), solver);
                CSetVar[] children = toCSetVars(randSets(nextInt(3) + 1), solver);
                CSetVar to = toCSetVar(randSet(), solver);
                Constraint constraint = Constraints.joinRelation(take.getSet(), mapSet(children), to.getSet());
                return pair(constraint, pair(triple(take.getSet(), mapSet(children), to.getSet()), false));
            }
        });
    }

    @Test(timeout = 60000)
    public void quickInjectiveTest() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>>() {
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>> setup(Solver solver) {
                CSetVar take = toCSetVar(randPositiveSet(), solver);
                CSetVar[] children = toCSetVars(randSets(nextInt(3) + 1), solver);
                CSetVar to = toCSetVar(randSet(), solver);
                Constraint constraint = Constraints.joinInjectiveRelation(take.getSet(), take.getCard(),
                        mapSet(children), mapCard(children), to.getSet(), to.getCard());
                return pair(LCF.and(constraint, SCF.all_disjoint(mapSet(children))),
                        pair(triple(take.getSet(), mapSet(children), to.getSet()), true));
            }
        });
    }

    @Test(timeout = 60000)
    public void testJoinRelation() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>>() {
            /*
             * import Control.Monad
             * import Data.List
             *
             * powerset = filterM (const [True, False])
             *
             * disjoint [] ys = True
             * disjoint (x:xs) ys = x `notElem` ys && disjoint xs ys
             *
             * positive = do
             *     take   <- powerset [0..2]
             *     child0 <- powerset [-1..1]
             *     child1 <- powerset [0..1]
             *     child2 <- powerset [-1..0]
             *     to <- powerset [0..1]
             *     guard $ to == sort (nub $ concat [[child0, child1, child2] !! i | i <- take])
             *     return (take, child0, child1, child2)
             *
             * negative= do
             *     take   <- powerset [0..2]
             *     child0 <- powerset [-1..1]
             *     child1 <- powerset [0..1]
             *     child2 <- powerset [-1..0]
             *     to <- powerset [0..1]
             *     guard $ to /= sort (nub $ concat [[child0, child1, child2] !! i | i <- take])
             *     return (take, child0, child1, child2)
             */
            @PositiveSolutions(576)
            @NegativeSolutions(3520)
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>> setup(Solver solver) {
                SetVar take = VF.set("take", 0, 2, solver);
                SetVar[] children = new SetVar[]{
                    VF.set("c1", -1, 1, solver),
                    VF.set("c2", 0, 1, solver),
                    VF.set("c3", -1, 0, solver)
                };
                SetVar to = VF.set("to", 0, 1, solver);
                Constraint constraint = Constraints.joinRelation(take, children, to);
                return pair(constraint, pair(triple(take, children, to), false));
            }
        });
    }

    @Test(timeout = 60000)
    public void testJoinInjectiveRelation() {
        randomizedTest(new TestCase<Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>>() {
            /*
             * import Control.Monad
             * import Data.List
             *
             * powerset = filterM (const [True, False])
             *
             * disjoint [] ys = True
             * disjoint (x:xs) ys = x `notElem` ys && disjoint xs ys
             *
             * positive = do
             *     take   <- powerset [0..2]
             *     child0 <- powerset [-1..1]
             *     child1 <- powerset [0..1]
             *     child2 <- powerset [-1..0]
             *     guard $ child0 `disjoint` child1 && child0 `disjoint` child2 && child1 `disjoint` child2
             *     to <- powerset [0..1]
             *     guard $ to == sort (concat [[child0, child1, child2] !! i | i <- take])
             *     return (take, child0, child1, child2)
             *
             * negative= do
             *     take   <- powerset [0..2]
             *     child0 <- powerset [-1..1]
             *     child1 <- powerset [0..1]
             *     child2 <- powerset [-1..0]
             *     to <- powerset [0..1]
             *     guard $ to /= sort (concat [[child0, child1, child2] !! i | i <- take]) || not (child0 `disjoint` child1 && child0 `disjoint` child2 && child1 `disjoint` child2)
             *     return (take, child0, child1, child2)
             */
            @PositiveSolutions(192)
            @NegativeSolutions(3904)
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>> setup(Solver solver) {
                SetVar take = VF.set("take", 0, 2, solver);
                IntVar takeCard = enforcedCardVar(take);
                SetVar[] children = new SetVar[]{
                    VF.set("c1", -1, 1, solver),
                    VF.set("c2", 0, 1, solver),
                    VF.set("c3", -1, 0, solver)
                };
                IntVar[] childrenCards = enforcedCardVars(children);
                SetVar to = VF.set("to", 0, 1, solver);
                IntVar toCard = enforcedCardVar(to);
                Constraint constraint = Constraints.joinInjectiveRelation(take, takeCard,
                        children, childrenCards, to, toCard);
                return pair(LCF.and(constraint, SCF.all_disjoint(children)),
                        pair(triple(take, children, to), true));
            }
        });
    }
}
