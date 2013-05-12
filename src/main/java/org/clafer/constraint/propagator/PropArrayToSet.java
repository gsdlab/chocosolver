package org.clafer.constraint.propagator;

import gnu.trove.set.hash.TIntHashSet;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import solver.variables.delta.IIntDeltaMonitor;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropArrayToSet extends Propagator<Variable> {

    private final IntVar[] ts;
    private final IIntDeltaMonitor[] tsD;
    private final SetVar s;
    private final SetDeltaMonitor sD;

    public PropArrayToSet(IntVar[] is, SetVar s) {
        super(buildArray(is, s), PropagatorPriority.BINARY);
        if (is.length == 0) {
            throw new IllegalArgumentException();
        }
        this.ts = is;
        this.tsD = PropUtil.monitorDeltas(is, aCause);
        this.s = s;
        this.sD = s.monitorDelta(aCause);

    }

    private static Variable[] buildArray(IntVar[] is, SetVar s) {
        Variable[] array = new Variable[is.length + 1];
        array[0] = s;
        System.arraycopy(is, 0, array, 1, is.length);
        return array;
    }

    private boolean isTVar(int idx) {
        return idx > 0;
    }

    private int getTVarIndex(int idx) {
        return idx - 1;
    }

    private boolean isSVar(int idx) {
        return idx == 0;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isTVar(vIdx)) {
            return EventType.REMOVE.mask + EventType.INSTANTIATE.mask;
        }
        assert isSVar(vIdx);
        return EventType.REMOVE_FROM_ENVELOPE.mask + EventType.ADD_TO_KER.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (s.getKernelSize() > ts.length) {
            contradiction(s, s + " is too large");
        }

        if (s.getKernelSize() == ts.length) {
            if (!s.instantiated()) {
                s.instantiateTo(PropUtil.iterateKer(s), aCause);
            }
        } else {
            int maxDisjoint = ts.length - s.getEnvelopeSize();
            if (maxDisjoint == 0) {
                if (!s.instantiated()) {
                    s.instantiateTo(PropUtil.iterateEnv(s), aCause);
                }
            }
        }
        PropUtil.intsSubsetEnv(ts, s, aCause);
        PropUtil.envSubsetInts(s, ts, aCause);
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        propagate(mask);
    }

    @Override
    public ESat isEntailed() {
        if (s.getKernelSize() > ts.length) {
            return ESat.FALSE;
        }
        boolean tsInstantiated = true;
        TIntHashSet values = new TIntHashSet();
        for (IntVar t : ts) {
            if (!PropUtil.canIntersect(t, s)) {
                return ESat.FALSE;
            }
            if (t.instantiated()) {
                values.add(t.getValue());
            } else {
                tsInstantiated = false;
            }

        }
        if (!PropUtil.isSubsetOfEnv(values, s)) {
            return ESat.FALSE;
        }
        if (tsInstantiated) {
            return PropUtil.isKerSubsetOf(s, values)
                    ? (s.instantiated() ? ESat.TRUE : ESat.UNDEFINED)
                    : ESat.FALSE;
        }
        return ESat.UNDEFINED;
    }

    public static void main(String[] args) {
    }
}
