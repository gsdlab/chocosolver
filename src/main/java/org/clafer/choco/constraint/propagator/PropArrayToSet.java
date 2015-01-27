package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.IIntDeltaMonitor;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropArrayToSet extends Propagator<Variable> {

    private final IntVar[] as;
    private final IIntDeltaMonitor[] asD;
    private final SetVar s;
    private final ISetDeltaMonitor sD;

    public PropArrayToSet(IntVar[] as, SetVar s) {
        super(buildArray(as, s), PropagatorPriority.TERNARY, true);
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
        return idx >= 1;
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
            return IntEventType.all();
        }
        assert isSVar(vIdx);
        return SetEventType.all();
    }

    private boolean findMate(int sEnv) throws ContradictionException {
        assert s.envelopeContains(sEnv);
        boolean inKer = s.kernelContains(sEnv);
        int mate = -1;
        for (int i = 0; i < as.length; i++) {
            if (as[i].contains(sEnv)) {
                // Found a second mate.
                if (mate != -1 || !inKer) {
                    mate = -2;
                    break;
                }
                mate = i;
            }
        }
        if (mate == -1) {
            // No mates.
            s.removeFromEnvelope(sEnv, aCause);
        } else if (mate != -2 && inKer) {
            // One mate.
            return as[mate].instantiateTo(sEnv, aCause);
        }
        return false;
    }

    private void findMates() throws ContradictionException {
        for (int i = s.getEnvelopeFirst(); i != SetVar.END; i = s.getEnvelopeNext()) {
            if (findMate(i)) {
                findMates();
                return;
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune as
        for (IntVar a : as) {
            PropUtil.domSubsetEnv(a, s, aCause);
        }
        // Prune s
        findMates();
        // Pick s
        for (IntVar a : as) {
            if (a.isInstantiated()) {
                s.addToKernel(a.getValue(), aCause);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSVar(idxVarInProp)) {
            sD.freeze();
            sD.forEach(pruneAOnSEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            sD.forEach(pickAOnSKer, SetEventType.ADD_TO_KER);
            sD.unfreeze();
        } else {
            assert isAVar(idxVarInProp);

            int id = getAVarIndex(idxVarInProp);
            if (IntEventType.isRemove(mask)
                    || (IntEventType.isInclow(mask) && as[id].getLB() > s.getEnvelopeFirst())
                    || IntEventType.isDecupp(mask)) {
                asD[id].freeze();
                asD[id].forEachRemVal(pruneSOnARem);
                asD[id].unfreeze();
            }
            if (as[id].isInstantiated()) {
                s.addToKernel(as[id].getValue(), aCause);
            }
        }
    }
    private final IntProcedure pruneAOnSEnv = new IntProcedure() {
        @Override
        public void execute(int sEnv) throws ContradictionException {
            for (IntVar a : as) {
                if (a.removeValue(sEnv, aCause) && a.isInstantiated()) {
                    s.addToKernel(a.getValue(), aCause);
                }
            }
        }
    };
    private final IntProcedure pickAOnSKer = new IntProcedure() {
        @Override
        public void execute(int sKer) throws ContradictionException {
            if (findMate(sKer)) {
                findMates();
            }
        }
    };
    private final IntProcedure pruneSOnARem = new IntProcedure() {
        @Override
        public void execute(int aRem) throws ContradictionException {
            if (s.envelopeContains(aRem)) {
                if (findMate(aRem)) {
                    findMates();
                }
            }
        }
    };

    @Override
    public ESat isEntailed() {
        if (s.getKernelSize() > as.length) {
            return ESat.FALSE;
        }
        for (IntVar a : as) {
            if (!PropUtil.isDomIntersectEnv(a, s)) {
                return ESat.FALSE;
            }
        }
        for (int i = s.getKernelFirst(); i != SetVar.END; i = s.getKernelNext()) {
            if (!PropUtil.domsContain(as, i)) {
                return ESat.FALSE;
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "arrayToSet({" + Util.commaSeparate(as) + "} = " + s + ")";
    }
}
