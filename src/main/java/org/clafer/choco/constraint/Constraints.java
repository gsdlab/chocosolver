package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropJoin;
import org.clafer.choco.constraint.propagator.PropJoinRef;
import org.clafer.choco.constraint.propagator.PropSelectN;
import org.clafer.choco.constraint.propagator.PropSingleton;
import org.clafer.choco.constraint.propagator.PropArrayToSet;
import org.clafer.choco.constraint.propagator.PropIntChannel;
import org.clafer.choco.constraint.propagator.PropIntNotMemberSet;
import org.clafer.choco.constraint.propagator.PropSetEqual;
import org.clafer.choco.constraint.propagator.PropSumSetN;
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

    private Constraints() {
    }

    public static Constraint singleton(SetVar svar, IntVar ivar) {
        return singleton(ivar, svar);
    }

    public static Constraint singleton(IntVar ivar, SetVar svar) {
        Constraint constraint = new Constraint(new Variable[]{ivar, svar}, ivar.getSolver());
        constraint.setPropagators(new PropSingleton(ivar, svar));
        return constraint;
    }

    public static Constraint arrayToSet(IntVar[] ivars, SetVar svar) {
        Variable[] vars = new Variable[ivars.length + 1];
        System.arraycopy(ivars, 0, vars, 0, ivars.length);
        vars[ivars.length] = svar;
        Constraint constraint = new Constraint(vars, svar.getSolver());
        constraint.setPropagators(new PropArrayToSet(ivars, svar));
        return constraint;
    }

    public static Constraint equal(SetVar s1, SetVar s2) {
        Constraint constraint = new Constraint(new SetVar[]{s1, s2}, s1.getSolver());
        constraint.setPropagators(new PropSetEqual(s1, s2));
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
        IntVar[] vars = new IntVar[bools.length + 1];
        System.arraycopy(bools, 0, vars, 0, bools.length);
        vars[bools.length] = n;
        Constraint constraint = new Constraint(vars, bools[0].getSolver());
        constraint.setPropagators(new PropSelectN(bools, n));
        return constraint;
    }

    public static Constraint intChannel(SetVar[] sets, IntVar[] ints) {
        Variable[] vars = new Variable[sets.length + ints.length];
        System.arraycopy(sets, 0, vars, 0, sets.length);
        System.arraycopy(ints, 0, vars, sets.length, ints.length);
        Constraint constraint = new Constraint(vars, sets[0].getSolver());
        constraint.setPropagators(new PropIntChannel(sets, ints));
        return constraint;
    }

    public static Constraint sumSetN(SetVar set, IntVar sum, int n) {
        Constraint constraint = new Constraint(new Variable[]{set, sum}, set.getSolver());
        constraint.setPropagators(new PropSumSetN(set, sum, n));
        return constraint;
    }

    public static Constraint notMember(IntVar element, SetVar set) {
        Constraint constraint = new Constraint(new Variable[]{element, set}, element.getSolver());
        constraint.setPropagators(new PropIntNotMemberSet(element, set));
        return constraint;
    }
}
