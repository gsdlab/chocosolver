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
public class ReifyEqualXYTest extends ConstraintTest<Triple<BoolVar, IntVar, IntVar>> {

    @Override
    protected void check(Triple<BoolVar, IntVar, IntVar> s) {
        assertEquals(s.getFst().getValue() == 1, s.getSnd().getValue() == s.getThd().getValue());
    }

    @Override
    protected void checkNot(Triple<BoolVar, IntVar, IntVar> s) {
        assertEquals(s.getFst().getValue() == 1, s.getSnd().getValue() != s.getThd().getValue());
    }
    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<BoolVar, IntVar, IntVar>>() {
            @Override
            public Pair<Constraint, Triple<BoolVar, IntVar, IntVar>> setup(Solver solver) {
                BoolVar reify = toBoolVar(randBool(), solver);
                IntVar i = toIntVar(randInt(), solver);
                IntVar j = toIntVar(randInt(), solver);
                return pair(Constraints.reifyEqual(reify, i, j), triple(reify, i, j));
            }
        });
    }

    @Test(timeout = 60000)
    public void testReifyEqualXY() {
        randomizedTest(new TestCase<Triple<BoolVar, IntVar, IntVar>>() {
            @PositiveSolutions(25)
            @NegativeSolutions(25)
            @Override
            public Pair<Constraint, Triple<BoolVar, IntVar, IntVar>> setup(Solver solver) {
                BoolVar reify = VF.bool("reify", solver);
                IntVar i = VF.enumerated("i", -2, 2, solver);
                IntVar j = VF.enumerated("j", -2, 2, solver);
                return pair(Constraints.reifyEqual(reify, i, j), triple(reify, i, j));
            }
        });
    }
}
