package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * Idempotent version of the one provided by the Choco library.
 *
 * @author jimmy
 */
public class PropSetUnion extends Propagator<SetVar> {

    private final SetVar[] sets;
    private final SetDeltaMonitor[] setsD;
    private final SetVar union;
    private final SetDeltaMonitor unionD;

    public PropSetUnion(SetVar[] sets, SetVar union) {
        super(Util.cons(union, sets), PropagatorPriority.LINEAR, true);
        this.sets = sets;
        this.setsD = PropUtil.monitorDeltas(sets, aCause);
        this.union = union;
        this.unionD = union.monitorDelta(aCause);
    }

    private boolean isSetVar(int idx) {
        return idx >= 1;
    }

    private int getSetVarIndex(int idx) {
        assert isSetVar(idx);
        return idx - 1;
    }

    private boolean isUnionVar(int idx) {
        return idx == 0;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
    }

    private void findMate(int unionEnv) throws ContradictionException {
        boolean inKer = union.kernelContains(unionEnv);
        int mate = -1;
        for (int j = 0; j < sets.length; j++) {
            if (sets[j].envelopeContains(unionEnv)) {
                // Found a second mate.
                if (mate != -1 || !inKer) {
                    mate = -2;
                    break;
                }
                mate = j;
            }
        }
        if (mate == -1) {
            // No mates.
            union.removeFromEnvelope(unionEnv, aCause);
        } else if (mate != -2 && inKer) {
            // One mate.
            sets[mate].addToKernel(unionEnv, aCause);
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (SetVar set : sets) {
            PropUtil.envSubsetEnv(set, union, aCause);
            PropUtil.kerSubsetKer(set, union, aCause);
        }
        PropUtil.envSubsetEnvs(union, sets, aCause);
        for (int i = union.getEnvelopeFirst(); i != SetVar.END; i = union.getEnvelopeNext()) {
            findMate(i);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            int id = getSetVarIndex(idxVarInProp);
            setsD[id].freeze();
            setsD[id].forEach(pruneUnionOnSetEnv, EventType.REMOVE_FROM_ENVELOPE);
            setsD[id].forEach(pickUnionOnSetKer, EventType.ADD_TO_KER);
            setsD[id].unfreeze();
        } else {
            assert isUnionVar(idxVarInProp);
            unionD.freeze();
            unionD.forEach(pruneSetOnUnionEnv, EventType.REMOVE_FROM_ENVELOPE);
            unionD.forEach(pickSetOnUnionKer, EventType.ADD_TO_KER);
            unionD.unfreeze();
        }
    }
    private final IntProcedure pruneUnionOnSetEnv = new IntProcedure() {
        @Override
        public void execute(int setEnv) throws ContradictionException {
            findMate(setEnv);
        }
    };
    private final IntProcedure pickUnionOnSetKer = new IntProcedure() {
        @Override
        public void execute(int setKer) throws ContradictionException {
            union.addToKernel(setKer, aCause);
        }
    };
    private final IntProcedure pruneSetOnUnionEnv = new IntProcedure() {
        @Override
        public void execute(int unionEnv) throws ContradictionException {
            for (SetVar set : sets) {
                set.removeFromEnvelope(unionEnv, aCause);
            }
        }
    };
    private final IntProcedure pickSetOnUnionKer = new IntProcedure() {
        @Override
        public void execute(int unionKer) throws ContradictionException {
            findMate(unionKer);
        }
    };

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        for (SetVar set : sets) {
            if (!PropUtil.isKerSubsetEnv(set, union)) {
                return ESat.FALSE;
            }
            allInstantiated = allInstantiated && set.instantiated();
        }
        if (!PropUtil.isKerSubsetEnvs(union, sets)) {
            return ESat.FALSE;
        }
        allInstantiated = allInstantiated && union.instantiated();
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return Util.intercalate(" ∪ ", sets) + " = " + union;
    }
}
