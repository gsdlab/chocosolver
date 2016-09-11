package org.clafer.choco.constraint;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.binary.PropEqualX_Y;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.util.ESat;
import org.clafer.choco.constraint.propagator.PropSetEqual;
import org.clafer.choco.constraint.propagator.PropSetNotEqual;
import org.clafer.choco.constraint.propagator.PropUtil;

/**
 *
 * @author jimmy
 */
public class SetEquality extends Constraint {

    private final SetVar left, right;
    private final IntVar leftCard, rightCard;
    private final boolean equal;

    public SetEquality(SetVar left, IntVar leftCard, boolean equal, SetVar right, IntVar rightCard) {
        super(equal ? "setEqual" : "setNotEqual", equal
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

    @Override
    public ESat isSatisfied() {
        ESat satisfied = super.isSatisfied();
        if (ESat.UNDEFINED.equals(satisfied)) {
            int intersectionSize = PropUtil.intersectionSize(left.getUB(), right.getUB());
            int possibleCard = leftCard.previousValue(intersectionSize + 1);
            while (possibleCard >= leftCard.getLB() && !rightCard.contains(possibleCard)) {
                possibleCard = leftCard.previousValue(possibleCard);
            }
            if (possibleCard < leftCard.getLB() || possibleCard < rightCard.getLB()) {
                return ESat.eval(!equal);
            }
        }
        return satisfied;
    }
}
