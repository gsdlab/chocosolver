package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
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
public class SelectNTest extends ConstraintTest<Pair<BoolVar[], IntVar>> {

    @Override
    protected void check(Pair<BoolVar[], IntVar> s) {
        for (int i = 0; i < s.getSnd().getValue(); i++) {
            assertEquals(1, s.getFst()[i].getValue());
        }
        for (int i = s.getSnd().getValue(); i < s.getFst().length; i++) {
            assertEquals(0, s.getFst()[i].getValue());
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<BoolVar[], IntVar>>() {
            @Override
            public Pair<Constraint, Pair<BoolVar[], IntVar>> setup(Solver solver) {
                BoolVar[] bools = toVars(randBools(nextInt(5) + 1), solver);
                IntVar n = toVar(randInt(0, bools.length), solver);
                return pair(Constraints.selectN(bools, n),
                        pair(bools, n));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSelectN() {
        randomizedTest(new TestCase<Pair<BoolVar[], IntVar>>() {
            @PositiveSolutions(5)
            @NegativeSolutions(75)
            @Override
            public Pair<Constraint, Pair<BoolVar[], IntVar>> setup(Solver solver) {
                BoolVar[] bools = VF.boolArray("bool", 4, solver);
                IntVar n = VF.enumerated("n", 0, 4, solver);
                return pair(Constraints.selectN(bools, n),
                        pair(bools, n));
            }
        });
    }
}
