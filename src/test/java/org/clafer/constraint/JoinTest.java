package org.clafer.constraint;

import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.set.SetConstraintsFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class JoinTest extends ConstraintTest {

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        SetVar take = VariableFactory.set("take", new int[]{0, 1, 2}, solver);
        SetVar[] children = new SetVar[3];
        for (int i = 0; i < children.length; i++) {
            children[i] = VariableFactory.set("child" + i, new int[]{0, 1, 2, 3, 4}, solver);
        }
        SetVar to = VariableFactory.set("to", new int[]{0, 1, 2, 3, 4}, solver);

        solver.post(Constraints.join(take, children, to));
        solver.post(SetConstraintsFactory.all_disjoint(children));

        assertEquals(8192, quickCheckModel(solver));
    }
}
