package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropIntNotMemberSet;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class SetNotMember extends Constraint {

    private final IntVar integer;
    private final SetVar set;

    public SetNotMember(IntVar integer, SetVar set) {
        super("SetNotMember", new PropIntNotMemberSet(integer, set));
        this.integer = integer;
        this.set = set;
    }

    @Override
    public Constraint makeOpposite() {
        return new SetMember(integer, set);
    }
}
