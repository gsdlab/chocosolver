package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

/**
 * result = [string !! i | i <- set].
 *
 * Pads result with extra negative one if necessary.
 *
 * @author jimmy
 */
public class PropFilterString extends Propagator<Variable> {

    private final SetVar set;
    // Sorted in decreasing order. Non-negatives
    private final IntVar[] string;
    private final IntVar[] result;

    public PropFilterString(SetVar set, IntVar[] string, IntVar[] result) {
        super(buildArray(set, string, result), PropagatorPriority.LINEAR, true);
        this.set = set;
        this.string = string;
        this.result = result;
    }

    public static Variable[] buildArray(SetVar set, IntVar[] string, IntVar[] result) {
        Variable[] array = new Variable[1 + string.length + result.length];
        array[0] = set;
        System.arraycopy(string, 0, array, 1, string.length);
        System.arraycopy(result, 0, array, 1 + string.length, result.length);
        return array;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isStringVar(int idx) {
        return idx > 0 && idx <= string.length;
    }

    private int getStringVarIndex(int idx) {
        assert isStringVar(idx);
        return idx - 1;
    }

    private boolean isResultVar(int idx) {
        return idx > string.length;
    }

    private int getResultVarIndex(int idx) {
        assert isResultVar(idx);
        return idx - 1 - string.length;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
        }
        return EventType.INT_ALL_MASK();
    }

    private void subset(IntVar a, IntVar b) throws ContradictionException {
        int ub = a.getUB();
        for (int i = a.getLB(); i <= ub; i = a.nextValue(i)) {
            if (!b.contains(i)) {
                a.removeValue(i, aCause);
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int index = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext(), index++) {
            if (!set.kernelContains(i)) {
                return;
            }
            subset(string[i], result[index]);
            subset(result[index], string[i]);
        }
        for (; index < result.length; index++) {
            result[index].instantiateTo(-1, aCause);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        if (!isCompletelyInstantiated()) {
            return ESat.UNDEFINED;
        }
        int index = 0;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext(), index++) {
            if (string[i].getValue() != result[index].getValue()) {
                return ESat.FALSE;
            }
        }
        for (; index < result.length; index++) {
            if (result[index].getValue() != -1) {
                return ESat.FALSE;
            }
        }
        return ESat.TRUE;
    }

    @Override
    public String toString() {
        return "filter(" + set + ", " + Arrays.toString(string) + ", " + Arrays.toString(result) + ")";
    }
}
