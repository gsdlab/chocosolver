package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class IfThenElseTest {

    @Input(solutions = 4)
    public Object testIfThenElse(Solver solver) {
        return $(bool("antecedent", solver),
                bool("consequent", solver),
                bool("alternative", solver));
    }

    @Check
    public void check(boolean antecedent, boolean consequent, boolean alternative) {
        assertTrue(antecedent ? consequent : alternative);
    }

    @ArcConsistent(opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        return Constraints.ifThenElse(antecedent, consequent, alternative);
    }
}
