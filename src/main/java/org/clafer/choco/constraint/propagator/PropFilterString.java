package org.clafer.choco.constraint.propagator;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;
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
 * result = [string !! i | i <- set].
 *
 * Pads result with extra negative one if necessary.
 *
 * @author jimmy
 */
public class PropFilterString extends Propagator<Variable> {

    private final SetVar set;
    private final int offset;
    // Sorted in decreasing order. Non-negatives
    private final IntVar[] string;
    private final IntVar[] result;

    public PropFilterString(SetVar set, int offset, IntVar[] string, IntVar[] result) {
        super(buildArray(set, string, result), PropagatorPriority.LINEAR, false);
        this.set = set;
        this.offset = offset;
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
        if (set.getKernelSize() > result.length) {
            contradiction(set, "Too many in kernel");
        }

        // The number of kernel elements seen.
        int minKer = 0;
        // The number of potential kernel elements seen.
        int maxKer = 0;
        TIntHashSet values = new TIntHashSet(Math.min(set.getEnvelopeSize(), result.length));
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            int x = i - offset;
            if (set.kernelContains(i)) {
                if (minKer == maxKer) {
                    subset(string[x], result[minKer]);
                    subset(result[minKer], string[x]);
                }
                minKer++;
                maxKer++;
                PropUtil.iterateDomain(string[x], values);
            } else {
                boolean found = false;
                for (int j = minKer; j < result.length && j <= maxKer; j++) {
                    if (PropUtil.domainIntersectDomain(string[x], result[j])) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    set.removeFromEnvelope(i, aCause);
                } else {
                    PropUtil.iterateDomain(string[x], values);
                    maxKer++;
                }
            }
        }
        for (int i = 0; i < minKer; i++) {
            PropUtil.domainSubsetOf(result[i], values, aCause);
        }
        values.add(-1);
        for (int i = minKer; i < result.length; i++) {
            PropUtil.domainSubsetOf(result[i], values, aCause);
        }
        values.clear();
        for (IntVar i : result) {
            PropUtil.iterateDomain(i, values);
        }
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            PropUtil.domainSubsetOf(string[i - offset], values, aCause);
        }
        if (set.instantiated()) {
            for (int i = set.getKernelSize(); i < result.length; i++) {
                result[i].instantiateTo(-1, aCause);
            }
        } else {
            int i = set.getKernelSize();
            for (; i < result.length && !result[i].contains(-1); i++) {
            }
            if (set.getEnvelopeSize() < i) {
                contradiction(set, "Too few in envelope");
            }
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
            if (index >= result.length) {
                return ESat.FALSE;
            }
            int x = i - offset;
            if (string[x].getValue() != result[index].getValue()) {
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
        return "filter(" + set + " >> " + offset + ", " + Arrays.toString(string) + ", " + Arrays.toString(result) + ")";
    }
}
