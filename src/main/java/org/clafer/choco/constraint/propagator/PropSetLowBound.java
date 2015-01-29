package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropSetLowBound extends Propagator<Variable> {

    private final SetVar set;
    private final ISetDeltaMonitor setD;
    private final IntVar bound;

    public PropSetLowBound(SetVar set, IntVar bound) {
        super(new Variable[]{set, bound}, PropagatorPriority.UNARY, false);
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
        return IntEventType.INCLOW.getMask() + IntEventType.instantiation();
    }

    private void boundEnv() throws ContradictionException {
        int lb = bound.getLB();
        int ub = bound.getUB();
        int i;
        for (i = set.getEnvelopeFirst(); i != SetVar.END && i < lb; i = set.getEnvelopeNext()) {
            set.removeFromEnvelope(i, aCause);
        }
        if (i != SetVar.END && i >= ub) {
            // The elements in the set's envelope are at least ub.
            setPassive();
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (set.getKernelSize() > 0) {
            bound.updateUpperBound(set.getKernelFirst(), aCause);
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
            bound.updateUpperBound(ker, aCause);
        }
    };

    @Override
    public ESat isEntailed() {
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            if (i < bound.getLB()) {
                return ESat.FALSE;
            }
        }
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (i < bound.getUB()) {
                return ESat.UNDEFINED;
            }
        }
        return ESat.TRUE;
    }

    @Override
    public String toString() {
        return set + ">>>=" + bound;
    }
}
