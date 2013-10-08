package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSortedSetsCard extends Propagator<Variable> {

    private final SetVar[] sets;
    private final IntVar[] cards;

    public PropSortedSetsCard(SetVar[] sets, IntVar[] cards) {
        super(buildArray(sets, cards), PropagatorPriority.QUADRATIC, false);
        if (sets.length != cards.length) {
            throw new IllegalArgumentException();
        }
        this.sets = sets;
        this.cards = cards;
    }

    public static Variable[] buildArray(SetVar[] sets, IntVar[] cards) {
        Variable[] array = new Variable[sets.length + cards.length];
        System.arraycopy(sets, 0, array, 0, sets.length);
        System.arraycopy(cards, 0, array, sets.length, cards.length);
        return array;
    }

    private boolean isSetVar(int idx) {
        return idx < sets.length;
    }

    private int getSetVarIndex(int idx) {
        return idx;
    }

    private boolean isCardVar(int idx) {
        return idx >= sets.length;
    }

    private int getCardVarIndex(int idx) {
        return idx - sets.length;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        if (isCardVar(vIdx)) {
            return EventType.BOUND.mask | EventType.INSTANTIATE.mask;
        }
        return EventType.VOID.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int low = 0;
        int high = 0;

        for (int i = 0; i < cards.length; i++) {
            IntVar card = cards[i];
            int newLow = low + card.getLB();
            int newHigh = high + card.getUB();
            SetVar set = sets[i];
            for (int j = set.getEnvelopeFirst(); j != SetVar.END; j = set.getEnvelopeNext()) {
                if (j < low || j >= newHigh) {
                    set.removeFromEnvelope(j, aCause);
                }
            }
            for (int j = high; j < newLow; j++) {
                set.addToKernel(j, aCause);
            }
            low = newLow;
            high = newHigh;
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        int low = 0;
        int high = 0;

        for (int i = 0; i < cards.length; i++) {
            IntVar card = cards[i];
            int newLow = low + card.getLB();
            int newHigh = high + card.getUB();
            SetVar set = sets[i];
            for (int j = set.getEnvelopeFirst(); j != SetVar.END; j = set.getEnvelopeNext()) {
                if (j < low || j >= newHigh) {
                    if (set.kernelContains(j)) {
                        return ESat.FALSE;
                    }
                }
            }
            for (int j = high; j < newLow; j++) {
                if (!set.envelopeContains(j)) {
                    return ESat.FALSE;
                }
            }
            low = newLow;
            high = newHigh;
        }

        return isCompletelyInstantiated() ? ESat.TRUE : ESat.FALSE;
    }

    @Override
    public String toString() {
        return "sorted(" + Arrays.toString(sets) + " || " + Arrays.toString(cards) + ")";
    }
}
