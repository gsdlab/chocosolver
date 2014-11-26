package org.clafer.choco.constraint.propagator;

import solver.Configuration;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import solver.variables.delta.ISetDeltaMonitor;
import solver.variables.events.IntEventType;
import solver.variables.events.SetEventType;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * {@code i ∈ set ⇒ i < bound}
 *
 * @author jimmy
 */
public class PropSetStrictHighBound extends Propagator<Variable> {

    private final SetVar set;
    private final ISetDeltaMonitor setD;
    private final IntVar bound;

    public PropSetStrictHighBound(SetVar set, IntVar bound) {
        super(new Variable[]{set, bound}, PropagatorPriority.UNARY, true);
        this.set = set;
        this.setD = set.monitorDelta(aCause);
        this.bound = bound;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isBoundVar(int idx) {
        return idx == 1;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.ADD_TO_KER.getMask();
        }
        assert isBoundVar(vIdx);
        return IntEventType.DECUPP.getMask() + IntEventType.instantiation();
    }

    private void boundEnv() throws ContradictionException {
        int lb = bound.getLB();
        int ub = bound.getUB();
        boolean smallerThanLb = true;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (i >= ub) {
                set.removeFromEnvelope(i, aCause);
            } else if (i >= lb) {
                smallerThanLb = false;
            }
        }
        if (smallerThanLb) {
            // The elements in the set's envelope are less than lb.
            setPassive();
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (set.getKernelSize() > 0) {
            bound.updateLowerBound(PropUtil.maxKer(set) + 1, aCause);
        }
        boundEnv();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            setD.freeze();
            setD.forEach(pruneBound, SetEventType.ADD_TO_KER);
            setD.unfreeze();
        } else {
            assert isBoundVar(idxVarInProp);
            boundEnv();
        }
    }

    private final IntProcedure pruneBound = new IntProcedure() {

        @Override
        public void execute(int ker) throws ContradictionException {
            bound.updateLowerBound(ker + 1, aCause);
        }
    };

    @Override
    public ESat isEntailed() {
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            if (i >= bound.getUB()) {
                return ESat.FALSE;
            }
        }
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (i >= bound.getLB()) {
                return ESat.UNDEFINED;
            }
        }
        return ESat.TRUE;
    }

    @Override
    public String toString() {
        return set + "<<<" + bound;
    }
}
