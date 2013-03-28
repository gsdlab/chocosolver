package org.clafer.constraint;

import org.clafer.collection.Appender;
import org.clafer.constraint.propagator.PropSelectN;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class Constraints {

    public static Constraint join(SetVar take, SetVar[] children, SetVar to) {
        Constraint con = new Constraint(
                Appender.<SetVar>build().add(take).addAll(children).add(to).toArray(),
                take.getSolver());
        con.setPropagators(new PropJoin(take, children, to));
        return con;
    }

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
