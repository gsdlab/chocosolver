package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import static org.junit.Assert.*;
import static org.junit.Assert.fail;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class AndTest extends ConstraintTest<BoolVar[]> {

    @Override
    protected void check(BoolVar[] vars) {
        for (BoolVar var : vars) {
            assertNotEquals(0, var.getValue());
        }
    }

    @Override
    protected void checkNot(BoolVar[] vars) {
        for (BoolVar var : vars) {
            if (var.getValue() == 0) {
                return;
            }
        }
        fail("All true.");
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<BoolVar[]>() {
            @Override
            public Pair<Constraint, BoolVar[]> setup(Solver solver) {
                BoolVar[] vars = toBoolVars(randBools(nextInt(3) + 1), solver);
                return pair(Constraints.and(vars), vars);
            }
        });
    }

    @Test(timeout = 60000)
    public void testAnd() {
        randomizedTest(new TestCase<BoolVar[]>() {
            @PositiveSolutions(1)
            @NegativeSolutions(31)
            @Override
            public Pair<Constraint, BoolVar[]> setup(Solver solver) {
                BoolVar[] vars = VF.boolArray("var", 5, solver);
                return pair(Constraints.and(vars), vars);
            }
        });
    }
}
