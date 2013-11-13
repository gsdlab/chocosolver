package org.clafer.choco.constraint;

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
public class NotMemberTest extends ConstraintTest<Pair<IntVar, SetVar>> {

    @Override
    protected void check(Pair<IntVar, SetVar> s) {
        assertFalse(Util.in(s.getFst().getValue(), s.getSnd().getValue()));
    }

    @Override
    protected void checkNot(Pair<IntVar, SetVar> s) {
        assertTrue(Util.in(s.getFst().getValue(), s.getSnd().getValue()));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<IntVar, SetVar>>() {
            @Override
            public Pair<Constraint, Pair<IntVar, SetVar>> setup(Solver solver) {
                IntVar element = toIntVar(randInt(), solver);
                SetVar set = toSetVar(randSet(), solver);
                return pair(Constraints.notMember(element, set), pair(element, set));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotMember() {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * positive = do
         *     i <- [-1..3]
         *     s <- powerset [0..5]
         *     guard $ i `notElem` s
         *     return (i, s)
         * 
         * negative = do
         *     i <- [-1..3]
         *     s <- powerset [0..5]
         *     guard $ i `elem` s
         *     return (i, s)
         */
        randomizedTest(new TestCase<Pair<IntVar, SetVar>>() {
            @PositiveSolutions(192)
            @NegativeSolutions(128)
            @Override
            public Pair<Constraint, Pair<IntVar, SetVar>> setup(Solver solver) {
                IntVar element = VF.enumerated("element", -1, 3, solver);
                SetVar set = VF.set("set", new int[]{0, 1, 2, 3, 4, 5}, solver);
                return pair(Constraints.notMember(element, set), pair(element, set));
            }
        });
    }
}
