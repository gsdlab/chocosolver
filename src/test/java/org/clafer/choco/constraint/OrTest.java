package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class OrTest extends ConstraintTest<BoolVar[]> {

    @Override
    protected void check(BoolVar[] vars) {
        for (BoolVar var : vars) {
            if (var.getValue() == 1) {
                return;
            }
        }
        fail("All false.");
    }

    @Override
    protected void checkNot(BoolVar[] vars) {
        for (BoolVar var : vars) {
            assertEquals(0, var.getValue());
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<BoolVar[]>() {
            @Override
            public Pair<Constraint, BoolVar[]> setup(Solver solver) {
                BoolVar[] vars = toBoolVars(randBools(nextInt(3) + 1), solver);
                return pair(Constraints.or(vars), vars);
            }
        });
    }

    @Test(timeout = 60000)
    public void testOr() {
        randomizedTest(new TestCase<BoolVar[]>() {
            @PositiveSolutions(31)
            @NegativeSolutions(1)
            @Override
            public Pair<Constraint, BoolVar[]> setup(Solver solver) {
                BoolVar[] vars = VF.boolArray("var", 5, solver);
                return pair(Constraints.or(vars), vars);
            }
        });
    }
}
