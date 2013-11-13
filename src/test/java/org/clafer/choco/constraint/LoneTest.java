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
public class LoneTest extends ConstraintTest<BoolVar[]> {

    @Override
    protected void check(BoolVar[] vars) {
        int count = 0;
        for (BoolVar var : vars) {
            if (var.getValue() == 1) {
                count++;
            }
        }
        assertTrue(count <= 1);
    }

    @Override
    protected void checkNot(BoolVar[] vars) {
        int count = 0;
        for (BoolVar var : vars) {
            if (var.getValue() == 1) {
                count++;
            }
        }
        assertTrue(count > 1);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<BoolVar[]>() {
            @Override
            public Pair<Constraint, BoolVar[]> setup(Solver solver) {
                BoolVar[] vars = toBoolVars(randBools(nextInt(3) + 1), solver);
                return pair(Constraints.lone(vars), vars);
            }
        });
    }

    @Test(timeout = 60000)
    public void testLone() {
        randomizedTest(new TestCase<BoolVar[]>() {
            @PositiveSolutions(6)
            @NegativeSolutions(26)
            @Override
            public Pair<Constraint, BoolVar[]> setup(Solver solver) {
                BoolVar[] vars = VF.boolArray("var", 5, solver);
                return pair(Constraints.lone(vars), vars);
            }
        });
    }
}