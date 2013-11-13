package org.clafer.choco.constraint;

import java.util.Arrays;
import org.clafer.collection.Pair;
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
public class SetEqualTest extends ConstraintTest<Pair<SetVar, SetVar>> {

    @Override
    protected void check(Pair<SetVar, SetVar> s) {
        assertArrayEquals(s.getFst().getValue(), s.getFst().getValue());
    }

    @Override
    protected void checkNot(Pair<SetVar, SetVar> s) {
        assertFalse(Arrays.equals(s.getFst().getValue(), s.getSnd().getValue()));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<SetVar, SetVar>>() {
            @Override
            public Pair<Constraint, Pair<SetVar, SetVar>> setup(Solver solver) {
                CSetVar s1 = toCSetVar(randSet(), solver);
                CSetVar s2 = toCSetVar(randSet(), solver);
                return pair(Constraints.equal(s1.getSet(), s1.getCard(), s2.getSet(), s2.getCard()),
                        pair(s1.getSet(), s2.getSet()));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetEqual() {
        randomizedTest(new TestCase<Pair<SetVar, SetVar>>() {
            /*
             * import Control.Monad
             *
             * powerset = filterM (const [True, False])
             *
             * positive = do
             *     s1 <- powerset [-1..2]
             *     s2 <- powerset [-2..1]
             *     guard $ s1 == s2
             *     return (s1, s2)
             *    
             * negative = do
             *     s1 <- powerset [-1..2]
             *     s2 <- powerset [-2..1]
             *     guard $ s1 /= s2
             *     return (s1, s2)
             */
            @PositiveSolutions(8)
            @NegativeSolutions(248)
            @Override
            public Pair<Constraint, Pair<SetVar, SetVar>> setup(Solver solver) {
                SetVar s1 = VF.set("s1", Util.range(-1, 2), solver);
                IntVar s1Card = enforcedCardVar(s1);
                SetVar s2 = VF.set("s2", Util.range(-2, 1), solver);
                IntVar s2Card = enforcedCardVar(s2);
                return pair(Constraints.equal(s1, s1Card, s2, s2Card), pair(s1, s2));
            }
        });
    }
}
