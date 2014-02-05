package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropSetEqual;
import org.clafer.choco.constraint.propagator.PropSetNotEqual;
import solver.constraints.Constraint;
import solver.constraints.Propagator;
import solver.constraints.binary.PropEqualX_Y;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class SetEquality extends Constraint {

    private final SetVar left, right;
    private final IntVar leftCard, rightCard;
    private final boolean equal;

    public SetEquality(SetVar left, IntVar leftCard, boolean equal, SetVar right, IntVar rightCard) {
        super("setEquality", equal
                ? new Propagator[]{new PropSetEqual(left, right), new PropEqualX_Y(leftCard, rightCard)}
                : new Propagator[]{new PropSetNotEqual(left, right)});
        this.left = left;
        this.leftCard = leftCard;
        this.equal = equal;
        this.right = right;
        this.rightCard = rightCard;
    }

    @Override
    public Constraint makeOpposite() {
        return new SetEquality(left, leftCard, !equal, right, rightCard);
    }
}
