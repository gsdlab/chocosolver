package org.clafer.choco.constraint;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.set.PropNotMemberIntSet;
import org.chocosolver.solver.constraints.set.PropNotMemberSetInt;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.choco.constraint.propagator.PropIntNotMemberSetCard;

/**
 *
 * @author jimmy
 */
public class SetNotMember extends Constraint {

    private final IntVar integer;
    private final SetVar set;

    public SetNotMember(IntVar integer, SetVar set) {
        super("SetNotMember",
                new PropNotMemberIntSet(integer, set),
                new PropNotMemberSetInt(integer, set),
                new PropIntNotMemberSetCard(integer, set, set.getCard()));
        this.integer = integer;
        this.set = set;
    }

    @Override
    public Constraint makeOpposite() {
        return new SetMember(integer, set);
    }

    @Override
    public String toString() {
        return integer + " not in " + set + " with " + set.getCard();
    }
}
