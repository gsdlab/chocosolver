package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class ReifyEqualXCTest extends ConstraintTest<Triple<BoolVar, IntVar, Integer>> {

    @Override
    protected void check(Triple<BoolVar, IntVar, Integer> s) {
        assertEquals(s.getFst().getValue() == 1, s.getSnd().getValue() == s.getThd().intValue());
    }

    @Override
    protected void checkNot(Triple<BoolVar, IntVar, Integer> s) {
        assertEquals(s.getFst().getValue() == 1, s.getSnd().getValue() != s.getThd().intValue());
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<BoolVar, IntVar, Integer>>() {
            @Override
            public Pair<Constraint, Triple<BoolVar, IntVar, Integer>> setup(Solver solver) {
                BoolVar reify = toVar(randBool(), solver);
                IntVar i = toVar(randInt(), solver);
                int j = nextIntBetween(-5, 5);
                return pair(Constraints.reifyEqual(reify, i, j), triple(reify, i, j));
            }
        });
    }

    @Test(timeout = 60000)
    public void testReifyEqualXC() {
        randomizedTest(new TestCase<Triple<BoolVar, IntVar, Integer>>() {
            @PositiveSolutions(21)
            @NegativeSolutions(21)
            @Override
            public Pair<Constraint, Triple<BoolVar, IntVar, Integer>> setup(Solver solver) {
                BoolVar reify = VF.bool("reify", solver);
                IntVar i = VF.enumerated("i", -10, 10, solver);
                int c = 4;
                return pair(Constraints.reifyEqual(reify, i, c), triple(reify, i, c));
            }
        });
    }
}