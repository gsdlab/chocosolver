package org.clafer.choco.constraint.propagator;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.common.Util;
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
import util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropArrayToSet extends Propagator<Variable> {

    private final IntVar[] as;
    private final IIntDeltaMonitor[] asD;
    private final SetVar s;
    private final SetDeltaMonitor sD;

    public PropArrayToSet(IntVar[] as, SetVar s) {
        super(buildArray(as, s), PropagatorPriority.BINARY);
        if (as.length == 0) {
            throw new IllegalArgumentException();
        }
        this.as = as;
        this.asD = PropUtil.monitorDeltas(as, aCause);
        this.s = s;
        this.sD = s.monitorDelta(aCause);
    }

    private static Variable[] buildArray(IntVar[] as, SetVar s) {
        Variable[] array = new Variable[as.length + 1];
        array[0] = s;
        System.arraycopy(as, 0, array, 1, as.length);
        return array;
    }

    private boolean isAVar(int idx) {
        return idx > 0;
    }

    private int getAVarIndex(int idx) {
        assert isAVar(idx);
        return idx - 1;
    }

    private boolean isSVar(int idx) {
        return idx == 0;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isAVar(vIdx)) {
            return EventType.INT_ALL_MASK();
        }
        assert isSVar(vIdx);
        return EventType.REMOVE_FROM_ENVELOPE.mask + EventType.ADD_TO_KER.mask;
    }

    private void checkKerSize() throws ContradictionException {
        if (s.getKernelSize() > as.length) {
            contradiction(s, s + " is too large");
        }
        if (s.getKernelSize() == as.length) {
            if (!s.instantiated()) {
                s.instantiateTo(PropUtil.iterateKer(s), aCause);
                PropUtil.intsSubsetEnv(as, s, aCause);
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune as
        PropUtil.intsSubsetEnv(as, s, aCause);
        // Prune s
        PropUtil.envSubsetInts(s, as, aCause);
        // Pick s
        for (IntVar a : as) {
            if (a.instantiated()) {
                s.addToKernel(a.getValue(), aCause);
            }
        }
        checkKerSize();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSVar(idxVarInProp)) {
            sD.freeze();
            sD.forEach(pruneAOnSEnv, EventType.REMOVE_FROM_ENVELOPE);
            sD.unfreeze();
            checkKerSize();
        } else {
            assert isAVar(idxVarInProp);

            int id = getAVarIndex(idxVarInProp);
            if (EventType.isRemove(mask)
                    || (EventType.isInclow(mask) && as[id].getLB() > s.getEnvelopeFirst())
                    || EventType.isDecupp(mask)) {
                asD[id].freeze();
                asD[id].forEach(pruneSOnARem, EventType.REMOVE);
                asD[id].unfreeze();
            }
            if (as[id].instantiated()) {
                s.addToKernel(as[id].getValue(), aCause);
                checkKerSize();
            }
        }
    }
    private final IntProcedure pruneAOnSEnv = new IntProcedure() {
        @Override
        public void execute(int sEnv) throws ContradictionException {
            for (IntVar a : as) {
                if (a.removeValue(sEnv, aCause) && a.instantiated()) {
                    s.addToKernel(a.getValue(), aCause);
                }
            }
        }
    };
    private final IntProcedure pruneSOnARem = new IntProcedure() {
        @Override
        public void execute(int aRem) throws ContradictionException {
            if (s.envelopeContains(aRem)) {
                for (IntVar a : as) {
                    if (a.contains(aRem)) {
                        return;
                    }
                }
                s.removeFromEnvelope(aRem, aCause);
            }
        }
    };

    @Override
    public ESat isEntailed() {
        if (s.getKernelSize() > as.length) {
            return ESat.FALSE;
        }
        boolean tsInstantiated = true;
        TIntHashSet values = new TIntHashSet();
        for (IntVar a : as) {
            if (!PropUtil.canIntersect(a, s)) {
                return ESat.FALSE;
            }
            if (a.instantiated()) {
                values.add(a.getValue());
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

    @Override
    public String toString() {
        return "{" + Util.commaSeparate(as) + "} = " + s;
    }
}
