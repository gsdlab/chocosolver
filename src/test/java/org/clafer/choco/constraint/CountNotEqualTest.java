package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class CountNotEqualTest {

    @Input(solutions = 625)
    public Object testCountNotEqual(Model model) {
        return $(2, model.intVarArray("array", 4, 0, 4), model.intVar("count", 0, 4));
    }

    @Input(solutions = 0)
    public Object testTrivialNoCount(Model model) {
        return $(2, model.intVarArray("array", 2, 0, 4), model.intVar(4));
    }

    @Input(solutions = 1)
    public Object testTrivialCount(Model model) {
        return $(2, model.intVarArray("array", 4, 0, 4), model.intVar(0));
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

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(int value, IntVar[] array, IntVar count) {
        return Constraints.countNotEqual(value, array, count);
    }
}
