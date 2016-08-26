package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropSetTernary;
import org.clafer.test.NoCard;
import static org.junit.Assert.assertArrayEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetTernaryNoCardTest {

    @Input(solutions = 512)
    public static Object testTernary(Model model) {
        // import Control.Monad
        //
        // powerset = filterM (const [True, False])
        //
        // solutions = do
        //     antecedent <- [True, False]
        //     result <- powerset [0..3]
        //     consequent <- powerset [0..3]
        //     alternative <- powerset [0..3]
        //     guard $ result == if antecedent then consequent else alternative
        //     return (antecedent, result, consequent, alternative)
        return $(model.boolVar("antecedent"),
                model.setVar("result", ker(), env(0, 1, 2, 3)),
                model.setVar("consequent", ker(), env(0, 1, 2, 3)),
                model.setVar("alternative", ker(), env(0, 1, 2, 3)));
    }

    @Check
    public void check(boolean antecedent, int[] result, int[] consequent, int[] alternative) {
        if (antecedent) {
            assertArrayEquals(result, consequent);
        } else {
            assertArrayEquals(result, alternative);
        }
    }

    @ArcConsistent
    @Idempotent
    @Test(timeout = 60000)
    public Constraint quickTest(
            BoolVar antecedent,
            @NoCard SetVar result,
            @NoCard SetVar consequent,
            @NoCard SetVar alternative) {
        return new Constraint("Ternary'", new PropSetTernary(antecedent, result, consequent, alternative));
    }
}
