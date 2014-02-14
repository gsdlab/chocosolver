package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.common.Util;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.Irs;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class JoinRelationTest extends ConstraintTest<Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>> {

    @Override
    protected void check(Pair<Triple<SetVar, SetVar[], SetVar>, Boolean> s) {
        SetVar take = s.getFst().getFst();
        SetVar[] children = s.getFst().getSnd();
        SetVar to = s.getFst().getThd();
        boolean injective = s.getSnd();

        TIntHashSet set = new TIntHashSet();

        for (int t : take.getValue()) {
            assertTrue(t >= 0 && t < children.length);
            for (int c : children[t].getValue()) {
                assertTrue(Util.in(c, to.getValue()));
                set.add(c);
            }
        }
        assertEquals(set.size(), to.getEnvelopeSize());
        if (injective) {
            int sum = 0;
            TIntHashSet disjointSet = new TIntHashSet();
            for (int t : take.getValue()) {
                SetVar child = children[t];
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
                return pair(constraint,
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
             * positive = do
             *     take   <- powerset [0..2]
             *     child0 <- powerset [-1..1]
             *     child1 <- powerset [0..1]
             *     child2 <- powerset [-1..0]
             *     let takeSet = concat [[child0, child1, child2] !! i | i <- take]
             *     to <- powerset [0..1]
             *     guard $ to == sort takeSet
             *     return (take, child0, child1, child2)
             *    
             * negative = do
             *     take   <- powerset [0..2]
             *     child0 <- powerset [-1..1]
             *     child1 <- powerset [0..1]
             *     child2 <- powerset [-1..0]
             *     let takeSet = concat [[child0, child1, child2] !! i | i <- take]
             *     to <- powerset [0..1]
             *     guard $ to /= sort takeSet
             *     return (take, child0, child1, child2)
             */
            @PositiveSolutions(504)
            @NegativeSolutions(3592)
            @Override
            public Pair<Constraint, Pair<Triple<SetVar, SetVar[], SetVar>, Boolean>> setup(Solver solver) {
                CSetVar take = toCSetVar(Irs.set("take", 0, 2), solver);
                CSetVar[] children = toCSetVars(new IrSetVar[]{
                    Irs.set("c1", -1, 1),
                    Irs.set("c2", 0, 1),
                    Irs.set("c3", -1, 0)
                }, solver);
                CSetVar to = toCSetVar(Irs.set("to", 0, 1), solver);
                Constraint constraint = Constraints.joinInjectiveRelation(take.getSet(), take.getCard(),
                        mapSet(children), mapCard(children), to.getSet(), to.getCard());
                return pair(constraint,
                        pair(triple(take.getSet(), mapSet(children), to.getSet()), true));
            }
        });
    }
}
