package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class IfThenElseTest {

    @Input(solutions = 4)
    public Object testIfThenElse(Model model) {
        return $(model.boolVar("antecedent"),
                model.boolVar("consequent"),
                model.boolVar("alternative"));
    }

    @Check
    public void check(boolean antecedent, boolean consequent, boolean alternative) {
        assertTrue(antecedent ? consequent : alternative);
    }

    @ArcConsistent(entailed = true, opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        return Constraints.ifThenElse(antecedent, consequent, alternative);
    }
}
