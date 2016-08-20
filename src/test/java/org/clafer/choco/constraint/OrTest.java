package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class OrTest {

    @Input(solutions = 31)
    public Object testOr(Model model) {
        return $(model.boolVarArray("bool", 5));
    }

    @Input(solutions = 1)
    public static Object testOneVar(Model model) {
        return $(model.boolVarArray("bool", 1));
    }

    @Input(solutions = 2)
    public static Object testTautology(Model model) {
        return $(new BoolVar[]{model.boolVar(true), model.boolVar(false), model.boolVar("bool")});
    }

    @Input(solutions = 0)
    public static Object testFalseTautology(Model model) {
        return $(new BoolVar[]{model.boolVar(false), model.boolVar(false)});
    }

    @Check
    public static void check(int[] bools) {
        assertTrue(Util.sum(bools) >= 1);
    }

    @ArcConsistent(opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty BoolVar[] bools) {
        return Constraints.or(bools);
    }
}
