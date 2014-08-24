package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class CountNotEqualTest {

    @Input(solutions = 625)
    public Object testCountNotEqual(Solver solver) {
        return $(2, enumeratedArray("array", 4, 0, 4, solver), enumerated("count", 0, 4, solver));
    }

    @Input(solutions = 0)
    public Object testTrivialNoCount(Solver solver) {
        return $(2, enumeratedArray("array", 2, 0, 4, solver), fixed(4, solver));
    }

    @Input(solutions = 1)
    public Object testTrivialCount(Solver solver) {
        return $(2, enumeratedArray("array", 4, 0, 4, solver), fixed(0, solver));
    }

    @Check
    public void check(int value, int[] array, int count) {
        int sum = 0;
        for (int v : array) {
            if (v != value) {
                sum++;
            }
        }
        assertEquals(count, sum);
    }

    @Test(timeout = 60000)
    public Constraint setup(int value, IntVar[] array, IntVar count) {
        return Constraints.countNotEqual(value, array, count);
    }
}
