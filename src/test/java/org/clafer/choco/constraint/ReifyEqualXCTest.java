package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ReifyEqualXCTest {

    @Input(solutions = 21)
    public Object testReifyEqualXC(Solver solver) {
        return $(bool("reify", solver),
                enumerated("i", -10, 10, solver),
                4);
    }

    @Check
    public void check(boolean reify, int v, int c) {
        assertEquals(reify, v == c);
    }

    @ArcConsistent(opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(BoolVar reify, IntVar v, int c) {
        return Constraints.reifyEqual(reify, v, c);
    }
}
