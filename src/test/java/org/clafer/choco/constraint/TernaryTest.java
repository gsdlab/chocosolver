package org.clafer.choco.constraint;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import static org.chocosolver.solver.variables.VariableFactory.*;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class TernaryTest {

    @Input(solutions = 32)
    public static Object testTernary(Solver solver) {
        return $(bool("antecedent", solver), enumerated("result", 0, 3, solver), enumerated("consequent", 0, 3, solver), enumerated("alternative", 0, 3, solver));
    }

    @Check
    public void check(boolean antecedent, int result, int consequent, int alternative) {
        if (antecedent) {
            assertEquals(result, consequent);
        } else {
            assertEquals(result, alternative);
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint quickTest(BoolVar antecedent, IntVar result, IntVar consequent, IntVar alternative) {
        return Constraints.ternary(antecedent, result, consequent, alternative);
    }
}
