package org.clafer.choco.constraint;

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
public class SetSumTest extends ConstraintTest<Triple<SetVar, IntVar, IntVar>> {

    @Override
    protected void check(Triple<SetVar, IntVar, IntVar> s) {
        int[] set = s.getFst().getValue();

        assertTrue(set.length <= s.getSnd().getValue());
        assertEquals(Util.sum(set), s.getThd().getValue());
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<SetVar, IntVar, IntVar>>() {
            @Override
            public Pair<Constraint, Triple<SetVar, IntVar, IntVar>> setup(Solver solver) {
                CSetVar set = toVar(randSet(), solver);
                IntVar sum = toVar(randInt(), solver);
                return pair(Constraints.setSum(set.getSet(), set.getCard(), sum),
                        triple(set.getSet(), set.getCard(), sum));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSumSet() {
        randomizedTest(new TestCase<Triple<SetVar, IntVar, IntVar>>() {
            /*import Control.Monad
             *        
             * powerset = filterM (const [True, False])
             *        
             * positive = do
             *     set <- powerset [-4..3]
             *     guard $ length set <= 2
             *     setSum <- [-4..4]
             *     guard $ sum set == setSum
             *     return set
             *   
             * negative = do
             *     set <- powerset [-4..3]
             *     setSum <- [-4..4]
             *     guard $ length set > 2 || sum set /= setSum
             *     return set
             */
            @PositiveSolutions(32)
            @NegativeSolutions(301)
            @Override
            public Pair<Constraint, Triple<SetVar, IntVar, IntVar>> setup(Solver solver) {
                SetVar set = VF.set("set", -4, 3, solver);
                IntVar setCard = enforcedCardVar(set, 0, 2);
                IntVar sum = VF.enumerated("sum", -4, 4, solver);
                return pair(Constraints.setSum(set, setCard, sum),
                        triple(set, setCard, sum));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSumNonPositiveSet() {
        randomizedTest(new TestCase<Triple<SetVar, IntVar, IntVar>>() {
            /*
             * import Control.Monad
             *
             * powerset = filterM (const [True, False])
             *         
             * positive = do
             *     set <- powerset [-5..0]
             *     guard $ length set <= 4
             *     setSum <- [-4..2]
             *     guard $ sum set == setSum
             *     return set
             *    
             * negative = do
             *     set <- powerset [-5..0]
             *     guard $ length set <= 4
             *     setSum <- [-4..2]
             *     guard $ sum set /= setSum
             *     return set
             */
            @PositiveSolutions(14)
            @NegativeSolutions(385)
            @Override
            public Pair<Constraint, Triple<SetVar, IntVar, IntVar>> setup(Solver solver) {
                SetVar set = VF.set("set", -5, 0, solver);
                IntVar setCard = enforcedCardVar(set, 0, 4);
                IntVar sum = VF.enumerated("sum", -4, 2, solver);
                return pair(Constraints.setSum(set, setCard, sum),
                        triple(set, setCard, sum));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSumKnown() {
        randomizedTest(new TestCase<Triple<SetVar, IntVar, IntVar>>() {
            /*
             * import Control.Monad
             *
             * powerset = filterM (const [True, False])
             *
             * positive = do
             *     set <- powerset [-1..3]
             *     guard $ length set >= 1 && length set <= 2
             *     guard $ sum set == 2
             *     return set
             *
             * negative = do
             *     set <- powerset [-1..3]
             *     guard $ length set >= 1 && length set <= 2
             *     guard $ sum set /= 2
             *     return set
             */
            @PositiveSolutions(3)
            @NegativeSolutions(12)
            @Override
            public Pair<Constraint, Triple<SetVar, IntVar, IntVar>> setup(Solver solver) {
                SetVar set = VF.set("set", -1, 3, solver);
                IntVar setCard = enforcedCardVar(set, 1, 2);
                IntVar sum = VF.enumerated("sum", 2, 2, solver);
                return pair(Constraints.setSum(set, setCard, sum),
                        triple(set, setCard, sum));
            }
        });
    }
}
