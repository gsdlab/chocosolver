package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class AndTest {

    @Input(solutions = 1)
    public Object testAnd(Model model) {
        return $(model.boolVarArray("bool", 5));
    }

    @Input(solutions = 1)
    public Object testOneVar(Model model) {
        return $(model.boolVarArray("bool", 1));
    }

    @Input(solutions = 1)
    public Object testTautology(Model model) {
        return $(new BoolVar[]{model.boolVar(true), model.boolVar(true)});
    }

    @Input(solutions = 0)
    public Object testFalseTautology(Model model) {
        return $(new BoolVar[]{model.boolVar(false), model.boolVar("bool")});
    }

    @Check
    public void check(int[] bools) {
        assertTrue(Util.sum(bools) == bools.length);
    }

    @ArcConsistent(opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty BoolVar[] bools) {
        return Constraints.and(bools);
    }
}
