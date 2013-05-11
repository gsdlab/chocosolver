package org.clafer.constraint;

import org.clafer.constraint.propagator.PropJoin;
import org.clafer.constraint.propagator.PropSelectN;
import org.clafer.constraint.propagator.PropSingleton;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;

/**
 *
 * @author jimmy
 */
public class Constraints {

    public static Constraint singleton(SetVar svar, IntVar ivar) {
        return singleton(ivar, svar);
    }

    public static Constraint singleton(IntVar ivar, SetVar svar) {
        Constraint con = new Constraint(new Variable[]{ivar, svar}, ivar.getSolver());
        con.setPropagators(new PropSingleton(ivar, svar));
        return con;
    }

    public static Constraint join(SetVar take, SetVar[] children, SetVar to) {
        SetVar[] vars = new SetVar[children.length + 2];
        vars[0] = take;
        vars[1] = to;
        System.arraycopy(children, 0, vars, 2, children.length);
        Constraint con = new Constraint(vars, take.getSolver());
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
