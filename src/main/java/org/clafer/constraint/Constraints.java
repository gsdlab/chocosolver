package org.clafer.constraint;

import org.clafer.constraint.propagator.PropSelectN;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class Constraints {

    public static Constraint increasing(IntVar[] vars) {
        return new Increasing(vars, vars[0].getSolver());
    }

    public static Constraint selectN(BoolVar[] bools, IntVar n) {
        IntVar[] init = new IntVar[bools.length + 1];
        System.arraycopy(bools, 0, init, 0, bools.length);
        init[bools.length] = n;

        Solver solver = bools[0].getSolver();
        Constraint constraint = new Constraint(init, solver);
        constraint.setPropagators(new PropSelectN(bools, n));
        return constraint;
    }
}
