package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.clafer.common.Util;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSetUnionCard extends Propagator<IntVar> {

    private final IntVar[] setCards;
    private final IntVar unionCard;

    private static PropagatorPriority computePriority(int nbvars) {
        switch (nbvars) {
            case 1:
                return PropagatorPriority.UNARY;
            case 2:
                return PropagatorPriority.BINARY;
            case 3:
                return PropagatorPriority.TERNARY;
            default:
                return PropagatorPriority.LINEAR;
        }
    }

    public PropSetUnionCard(IntVar[] setCards, IntVar unionCard) {
        super(Util.snoc(setCards, unionCard), computePriority(setCards.length), false);
        this.setCards = setCards;
        this.unionCard = unionCard;
    }

    private boolean isSetCardVar(int idx) {
        return idx < setCards.length;
    }

    private int getSetCardVarIndex(int idx) {
        return idx;
    }

    private boolean isUnionCardVar(int idx) {
        return idx == setCards.length;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return IntEventType.boundAndInst();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        boolean changed;
        do {
            changed = false;
            int min = 0;
            int max = 0;
            for (IntVar setCard : setCards) {
                min = Math.max(min, setCard.getLB());
                max += setCard.getUB();
            }
            unionCard.updateLowerBound(min, aCause);
            unionCard.updateUpperBound(max, aCause);
            int lb = unionCard.getLB();
            int ub = unionCard.getUB();

            for (IntVar setCard : setCards) {
                changed |= setCard.updateUpperBound(ub, aCause);
                changed |= setCard.updateLowerBound(lb - max + setCard.getUB(), aCause);
            }
        } while (changed);
    }

    @Override
    public ESat isEntailed() {
        int min = 0;
        int max = 0;
        for (IntVar setCard : setCards) {
            min = Math.max(min, setCard.getLB());
            max += setCard.getUB();
        }
        if (min > unionCard.getUB() || max < unionCard.getLB()) {
            return ESat.FALSE;
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "unionCard(" + Arrays.toString(setCards) + ", " + unionCard + ")";
    }
}
