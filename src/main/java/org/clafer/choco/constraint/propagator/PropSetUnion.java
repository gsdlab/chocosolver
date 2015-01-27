package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import org.chocosolver.solver.ICause;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 * Idempotent version of the one provided by the Choco library.
 *
 * @author jimmy
 */
public class PropSetUnion extends Propagator<SetVar> {

    private final SetVar[] sets;
    private final ISetDeltaMonitor[] setsD;
    private final SetVar union;
    private final ISetDeltaMonitor unionD;

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
        return SetEventType.all();
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

    private static boolean isKerSubsetEnvs(SetVar set, SetVar[] envs) {
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            if (!PropUtil.envsContain(envs, i)) {
                return false;
            }
        }
        return true;
    }

    private static void envSubsetEnvs(SetVar set, SetVar[] envs, ICause propagator) throws ContradictionException {
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (!PropUtil.envsContain(envs, i)) {
                set.removeFromEnvelope(i, propagator);
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (SetVar set : sets) {
            PropUtil.envSubsetEnv(set, union, aCause);
            PropUtil.kerSubsetKer(set, union, aCause);
        }
        envSubsetEnvs(union, sets, aCause);
        for (int i = union.getEnvelopeFirst(); i != SetVar.END; i = union.getEnvelopeNext()) {
            findMate(i);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            int id = getSetVarIndex(idxVarInProp);
            setsD[id].freeze();
            setsD[id].forEach(pruneUnionOnSetEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            setsD[id].forEach(pickUnionOnSetKer, SetEventType.ADD_TO_KER);
            setsD[id].unfreeze();
        } else {
            assert isUnionVar(idxVarInProp);
            unionD.freeze();
            unionD.forEach(pruneSetOnUnionEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            unionD.forEach(pickSetOnUnionKer, SetEventType.ADD_TO_KER);
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
            allInstantiated = allInstantiated && set.isInstantiated();
        }
        if (!isKerSubsetEnvs(union, sets)) {
            return ESat.FALSE;
        }
        allInstantiated = allInstantiated && union.isInstantiated();
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return Util.intercalate(" ∪ ", sets) + " = " + union;
    }
}
