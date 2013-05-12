package org.clafer.constraint;

import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class SelectNTest extends ConstraintTest {

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        BoolVar[] bools = VariableFactory.boolArray("bool", 20, solver);
        IntVar n = VariableFactory.enumerated("n", 0, 20, solver);

        solver.post(Constraints.selectN(bools, n));

        assertEquals(21, randomizeStrategy(solver).findAllSolutions());
    }
}
