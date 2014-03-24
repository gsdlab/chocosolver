package org.clafer.choco.constraint;

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
public class SetDifferenceTest extends ConstraintTest<Triple<SetVar, SetVar, SetVar>> {

    @Override
    protected void check(Triple<SetVar, SetVar, SetVar> s) {
        TIntHashSet answer = new TIntHashSet(s.getFst().getValue());
        answer.removeAll(s.getSnd().getValue());
        assertEquals(answer, new TIntHashSet(s.getThd().getValue()));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<SetVar, SetVar, SetVar>>() {
            @Override
            public Pair<Constraint, Triple<SetVar, SetVar, SetVar>> setup(Solver solver) {
                CSetVar minuend = toVar(randSet(), solver);
                CSetVar subtrahend = toVar(randSet(), solver);
                CSetVar difference = toVar(randSet(), solver);

                return (pair(Constraints.difference(minuend.getSet(), minuend.getCard(),
                        subtrahend.getSet(), subtrahend.getCard(), difference.getSet(), difference.getCard()),
                        triple(minuend.getSet(), subtrahend.getSet(), difference.getSet())));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetDifference() {
        randomizedTest(new TestCase<Triple<SetVar, SetVar, SetVar>>() {
            /*
             * import Control.Monad
             * import Data.List
             *
             * powerset = filterM (const [True, False])
             *
             * positive = do
             *     minuend <- powerset [-2..1]
             *     subtrahend <- powerset [-1..2]
             *     difference <- powerset [-1..2]
             *     guard $ difference == deleteFirstsBy (==) minuend subtrahend
             *     return (minuend, subtrahend, difference)
             * 
             * negative = do
             *     minuend <- powerset [-2..1]
             *     subtrahend <- powerset [-1..2]
             *     difference <- powerset [-1..2]
             *     guard $ difference /= deleteFirstsBy (==) minuend subtrahend
             *     return (minuend, subtrahend, difference)
             */
            @PositiveSolutions(128)
            @NegativeSolutions(3968)
            @Override
            public Pair<Constraint, Triple<SetVar, SetVar, SetVar>> setup(Solver solver) {
                SetVar minuend = VF.set("Minuend", -2, 1, solver);
                IntVar minuendCard = enforcedCardVar(minuend);
                SetVar subtrahend = VF.set("Subtrahend", -1, 2, solver);
                IntVar subtrahendCard = enforcedCardVar(subtrahend);
                SetVar difference = VF.set("Difference", -1, 2, solver);
                IntVar differenceCard = enforcedCardVar(difference);

                return (pair(Constraints.difference(minuend, minuendCard,
                        subtrahend, subtrahendCard, difference, differenceCard),
                        triple(minuend, subtrahend, difference)));
            }
        });
    }
}
