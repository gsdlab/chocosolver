package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSortedSets extends Propagator<SetVar> {

    private final SetVar[] sets;

    public PropSortedSets(SetVar[] sets) {
        super(sets, PropagatorPriority.LINEAR, false);
        this.sets = sets;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return EventType.ADD_TO_KER.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Right now, it don't does a very simple propagation. It does not
        // enforce sorted sets by itself, requires PropSortedSetsCard in conjunction.
        // Can make it better if necessary but might turn out slower.
        for (int i = 0; i < sets.length; i++) {
            SetVar set = sets[i];
            int cur = set.getKernelFirst();
            if (cur != SetVar.END) {
                for (int next = set.getKernelNext(); next != SetVar.END; next = set.getKernelNext()) {
                    for (int j = cur + 1; j < next; j++) {
                        set.addToKernel(j, aCause);
                    }
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        for (int i = 0; i < sets.length; i++) {
            SetVar set = sets[i];
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
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "sortedSets(" + Arrays.toString(sets) + ")";
    }
}
