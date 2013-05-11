package org.clafer.constraint;

import java.util.Arrays;
import org.clafer.constraint.propagator.PropJoin;
import org.clafer.constraint.propagator.PropJoinRef;
import org.clafer.constraint.propagator.PropSelectN;
import org.clafer.constraint.propagator.PropSingleton;
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
        Constraint constraint = new Constraint(new Variable[]{ivar, svar}, ivar.getSolver());
        constraint.setPropagators(new PropSingleton(ivar, svar));
        return constraint;
    }

    public static Constraint join(SetVar take, SetVar[] children, SetVar to) {
        SetVar[] vars = new SetVar[children.length + 2];
        vars[0] = take;
        vars[1] = to;
        System.arraycopy(children, 0, vars, 2, children.length);
        Constraint constraint = new Constraint(vars, take.getSolver());
        constraint.setPropagators(new PropJoin(take, children, to));
        return constraint;
    }

    public static Constraint joinRef(SetVar take, IntVar[] refs, SetVar to) {
        Variable[] vars = new Variable[refs.length + 2];
        vars[0] = take;
        vars[1] = to;
        System.arraycopy(refs, 0, vars, 2, refs.length);
        Constraint constraint = new Constraint(vars, take.getSolver());
        constraint.setPropagators(new PropJoinRef(take, refs, to));
        return constraint;
    }

    public static Constraint increasing(IntVar[] vars) {
        return new Increasing(vars, vars[0].getSolver());
    }

    public static Constraint selectN(BoolVar[] bools, IntVar n) {
        IntVar[] init = new IntVar[bools.length + 1];
        System.arraycopy(bools, 0, init, 0, bools.length);
        init[bools.length] = n;
        Constraint constraint = new Constraint(init, bools[0].getSolver());
        constraint.setPropagators(new PropSelectN(bools, n));
        return constraint;
    }
}
