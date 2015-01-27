package org.clafer.choco.constraint;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.set.PropIntEnumMemberSet;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class SetMember extends Constraint {

    private final IntVar integer;
    private final SetVar set;

    public SetMember(IntVar integer, SetVar set) {
        super("SetMember", new PropIntEnumMemberSet(set, integer));
        this.integer = integer;
        this.set = set;
    }

    @Override
    public Constraint makeOpposite() {
        return new SetNotMember(integer, set);
    }
}
