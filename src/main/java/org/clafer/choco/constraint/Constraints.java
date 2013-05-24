package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropAnd;
import org.clafer.choco.constraint.propagator.PropJoin;
import org.clafer.choco.constraint.propagator.PropJoinRef;
import org.clafer.choco.constraint.propagator.PropSelectN;
import org.clafer.choco.constraint.propagator.PropSingleton;
import org.clafer.choco.constraint.propagator.PropArrayToSet;
import org.clafer.choco.constraint.propagator.PropIntChannel;
import org.clafer.choco.constraint.propagator.PropIntNotMemberSet;
import org.clafer.choco.constraint.propagator.PropLone;
import org.clafer.choco.constraint.propagator.PropOne;
import org.clafer.choco.constraint.propagator.PropOr;
import org.clafer.choco.constraint.propagator.PropSetEqual;
import org.clafer.choco.constraint.propagator.PropSetSumN;
import org.clafer.choco.constraint.propagator.PropUnion;
import org.clafer.common.Util;
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

    /**
     * <p>
     * Sum the set with a maximum cardinality. More efficient than the standard
     * operation in the Choco library when n is relatively small.
     * </p>
     * <p>
     * For example:
     * <pre>
     *   Animal 2
     *     Age -> integer
     *   [Animal.Age = 1000]
     * </pre>
     * </p>
     * <p> {@code Animal.Age} is a set with a very large envelope. However, due
     * to static analysis of the model, it is easy to see that the cardinality
     * can be no larger than 2, hence n=2. Once the first integer x is selected
     * for the set, the second integer 1000 - x is already determined due to
     * n=2. Since the Choco library's setSum constraint is not given n, it
     * cannot make this deduction.
     * </p>
     *
     * @param set the set of integers
     * @param sum the sum of the set
     * @param n the maximum cardinality of the set
     * @return a constraint where |set| &le n and sum = Σ set
     */
    public static Constraint setSumN(SetVar set, IntVar sum, int n) {
        Constraint constraint = new Constraint(new Variable[]{set, sum}, set.getSolver());
        constraint.setPropagators(new PropSetSumN(set, sum, n));
        return constraint;
    }

    public static Constraint notMember(IntVar element, SetVar set) {
        Constraint constraint = new Constraint(new Variable[]{element, set}, element.getSolver());
        constraint.setPropagators(new PropIntNotMemberSet(element, set));
        return constraint;
    }

    public static Constraint and(BoolVar... vars) {
        Constraint constraint = new Constraint(vars, vars[0].getSolver());
        constraint.setPropagators(new PropAnd(vars));
        return constraint;
    }

    public static Constraint lone(BoolVar... vars) {
        Constraint constraint = new Constraint(vars, vars[0].getSolver());
        constraint.setPropagators(new PropLone(vars));
        return constraint;
    }

    public static Constraint one(BoolVar... vars) {
        Constraint constraint = new Constraint(vars, vars[0].getSolver());
        constraint.setPropagators(new PropOne(vars));
        return constraint;
    }

    public static Constraint or(BoolVar... vars) {
        Constraint constraint = new Constraint(vars, vars[0].getSolver());
        constraint.setPropagators(new PropOr(vars));
        return constraint;
    }
    
        public static Constraint union(SetVar[] sets, SetVar union) {
        Constraint constraint = new Constraint(Util.cons(union, sets), union.getSolver());
        constraint.setPropagators(new PropUnion(sets, union));
        return constraint;
    }
}
