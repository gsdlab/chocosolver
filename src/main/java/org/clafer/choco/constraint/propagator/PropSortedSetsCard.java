package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.clafer.choco.constraint.Constraints;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.constraints.set.SCF;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.VF;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSortedSetsCard extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

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

    private static Variable[] buildArray(SetVar[] sets, IntVar[] cards) {
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
            return IntEventType.all();
        }
        return SetEventType.all();
    }

    public int maxKer(SetVar set) {
        int max = SetVar.END;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            max = i;
        }
        return max;
    }

    public int maxEnv(SetVar set) {
        int max = SetVar.END;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            max = i;
        }
        return max;
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

        boolean changed;
        int boundary = 0;
        boolean hasBoundary;
        do {
            changed = false;
            hasBoundary = false;
            for (int i = sets.length - 1; i >= 0; i--) {
                SetVar set = sets[i];
                IntVar card = cards[i];
                if (hasBoundary) {
                    int lb = cards[i].getLB();
                    for (int j = 1; j <= lb; j++) {
                        set.addToKernel(boundary - j, aCause);
                    }
                }
                if (card.getUB() > 0) {
                    hasBoundary = false;
                }
                if (set.getKernelSize() > 0) {
                    boundary = set.getKernelFirst();
                    if (boundary == set.getEnvelopeFirst()) {
                        hasBoundary = true;
                    }
                }
            }
            hasBoundary = false;
            for (int i = 0; i < sets.length; i++) {
                SetVar set = sets[i];
                if (hasBoundary) {
                    int lb = cards[i].getLB();
                    for (int j = 1; j <= lb; j++) {
                        changed |= set.addToKernel(boundary + j, aCause);
                    }
                }
                if (cards[i].getUB() > 0) {
                    hasBoundary = false;
                }
                if (set.getKernelSize() > 0) {
                    boundary = maxKer(set);
                    if (boundary == maxEnv(set)) {
                        hasBoundary = true;
                    }
                }
            }
        } while (changed);
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

        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "sortedSetsCard(" + Arrays.toString(sets) + " || " + Arrays.toString(cards) + ")";
    }
}
