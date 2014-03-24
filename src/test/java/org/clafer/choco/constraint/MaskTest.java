package org.clafer.choco.constraint;

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
public class MaskTest extends ConstraintTest<Triple<SetVar, SetVar, Pair<Integer, Integer>>> {

    @Override
    protected void check(Triple<SetVar, SetVar, Pair<Integer, Integer>> s) {
        SetVar set = s.getFst();
        SetVar masked = s.getSnd();
        int from = s.getThd().getFst();
        int to = s.getThd().getSnd();
        int count = 0;
        for (int i = 0; i < to - from; i++) {
            assertEquals(set.kernelContains(from + i), masked.kernelContains(i));
            count += masked.kernelContains(i) ? 1 : 0;
        }
        assertEquals(count, masked.getKernelSize());
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<SetVar, SetVar, Pair<Integer, Integer>>>() {
            @Override
            public Pair<Constraint, Triple<SetVar, SetVar, Pair<Integer, Integer>>> setup(Solver solver) {
                CSetVar set = toVar(randSet(), solver);
                CSetVar masked = toVar(randPositiveSet(), solver);
                int a = nextIntBetween(-5, 5);
                int b = nextIntBetween(-5, 5);
                int from = Math.min(a, b);
                int to = Math.max(a, b);
                return pair(Constraints.mask(set.getSet(), set.getCard(),
                        masked.getSet(), masked.getCard(), from, to),
                        triple(set.getSet(), masked.getSet(), pair(from, to)));
            }
        });
    }

    @Test(timeout = 60000)
    public void testMask() {
        randomizedTest(new TestCase<Triple<SetVar, SetVar, Pair<Integer, Integer>>>() {
            /*
             * import Control.Monad
             * import Data.List
             *         
             * powerset = filterM (const [True, False])
             *         
             * positive = do
             *     set <- powerset [1..6]
             *     masked <- powerset [0..3]
             *     guard $ masked == [s - 2 | s <- set, s >= 2 && s < 5]
             *     return (set, masked)
             *    
             * negative = do
             *     set <- powerset [1..6]
             *     masked <- powerset [0..3]
             *     guard $ masked /= [s - 2 | s <- set, s >= 2 && s < 5]
             *     return (set, masked)
             */
            @PositiveSolutions(64)
            @NegativeSolutions(960)
            @Override
            public Pair<Constraint, Triple<SetVar, SetVar, Pair<Integer, Integer>>> setup(Solver solver) {
                SetVar set = VF.set("set", 1, 6, solver);
                IntVar setCard = enforcedCardVar(set);
                SetVar masked = VF.set("masked", 0, 3, solver);
                IntVar maskedCard = enforcedCardVar(masked);
                int from = 2;
                int to = 5;
                return pair(Constraints.mask(set, setCard, masked, maskedCard, from, to),
                        triple(set, masked, pair(from, to)));
            }
        });
    }
}
