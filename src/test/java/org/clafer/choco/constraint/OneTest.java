package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class OneTest {

    @Input(solutions = 5)
    public Object testOne(Model model) {
        return $(model.boolVarArray("bool", 5));
    }

    @Input(solutions = 1)
    public static Object testOneVar(Model model) {
        return $(model.boolVarArray("bool", 1));
    }

    @Input(solutions = 1)
    public static Object testTautology(Model model) {
        return $(new BoolVar[]{model.boolVar(true), model.boolVar(false)});
    }

    @Input(solutions = 0)
    public static Object testFalseTautology(Model model) {
        return $(new BoolVar[]{model.boolVar(true), model.boolVar(true), model.boolVar("bool")});
    }

    @Check
    public static void check(int[] bools) {
        assertEquals(1, Util.sum(bools));
    }

    @ArcConsistent(entailed = true)
    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty BoolVar[] bools) {
        return Constraints.one(bools);
    }
}
