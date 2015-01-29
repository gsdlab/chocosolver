package org.clafer.choco.constraint;

import solver.constraints.Constraint;
import solver.constraints.set.PropIntMemberSet;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class SetMember extends Constraint {

    private final IntVar integer;
    private final SetVar set;

    public SetMember(IntVar integer, SetVar set) {
        super("SetMember", new PropIntMemberSet(set, integer));
        this.integer = integer;
        this.set = set;
    }

    @Override
    public Constraint makeOpposite() {
        return new SetNotMember(integer, set);
    }
}
