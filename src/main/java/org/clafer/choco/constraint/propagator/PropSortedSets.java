package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSortedSets extends Propagator<Variable> {

    private final SetVar[] sets;
    private final IntVar[] cards;

    public PropSortedSets(SetVar[] sets, IntVar[] cards) {
        super(buildArray(sets, cards), PropagatorPriority.LINEAR, false);
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

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Right now, it does a very simple propagation. It does not
        // enforce sorted sets by itself, requires PropSortedSetsCard in conjunction.
        // Can make it better if necessary but might turn out slower.
        int min = Integer.MIN_VALUE;
        for (int i = 0; i < sets.length; i++) {
            SetVar set = sets[i];
            IntVar card = cards[i];
            for (int j = set.getEnvelopeFirst(); j != SetVar.END && j <= min; j = set.getEnvelopeNext()) {
                set.removeFromEnvelope(j, aCause);
            }
            for (int j = set.getKernelFirst(); j != SetVar.END; j = set.getKernelNext()) {
                min = j;
            }
            if (card.getLB() > 0) {
                int lim = card.getLB() - 1;
                if (lim < 0 || lim >= set.getEnvelopeSize()) {
                    contradiction(card, "too small or large");
                }
                int j = set.getEnvelopeFirst();
                for (int k = 0; k < lim; k++) {
                    j = set.getEnvelopeNext();
                }
                min = Math.max(min, j);
            }
        }
        int max = Integer.MAX_VALUE;
        for (int i = sets.length - 1; i >= 0; i--) {
            SetVar set = sets[i];
            IntVar card = cards[i];
            for (int j = set.getEnvelopeFirst(); j != SetVar.END; j = set.getEnvelopeNext()) {
                if (j >= max) {
                    set.removeFromEnvelope(j, aCause);
                }
            }
            if (set.getKernelSize() > 0) {
                max = set.getKernelFirst();
            }
            if (card.getLB() > 0) {
                int lim = set.getEnvelopeSize() - card.getLB();
                if (lim < 0 || lim >= set.getEnvelopeSize()) {
                    contradiction(card, "too small or large");
                }
                int j = set.getEnvelopeFirst();
                for (int k = 0; k < lim; k++) {
                    j = set.getEnvelopeNext();
                }
                max = Math.min(max, j);
            }
        }
    }

    @Override
    public ESat isEntailed() {
        boolean completelyInstantiated = true;
        for (int i = 0; i < sets.length; i++) {
            SetVar set = sets[i];
            completelyInstantiated &= set.isInstantiated();
            int cur = set.getKernelFirst();
            if (cur != SetVar.END) {
                for (int next = set.getKernelNext(); next != SetVar.END; next = set.getKernelNext()) {
                    for (int j = cur + 1; cur < next; cur++) {
                        if (!set.envelopeContains(j)) {
                            return ESat.FALSE;
                        }
                    }
                }
            }
        }
        return completelyInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "sortedSets(" + Arrays.toString(sets) + ")";
    }
}
