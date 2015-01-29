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
public class ReifyEqualXYTest {

    @Input(solutions = 25)
    public Object testReifyEqualXY(Solver solver) {
        return $(bool("reify", solver),
                enumerated("i", -2, 2, solver),
                enumerated("j", -2, 2, solver));
    }

    @Check
    public void check(boolean reify, int v1, int v2) {
        assertEquals(reify, v1 == v2);
    }

    @ArcConsistent(opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(BoolVar reify, IntVar v1, IntVar v2) {
        return Constraints.reifyEqual(reify, v1, v2);
    }
}
