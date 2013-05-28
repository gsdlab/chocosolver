package org.clafer.choco.constraint.propagator;

import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * More efficient than the provided PropAllDiff. Idempotent unlike the
 * PropAllDiff.
 *
 * @author jimmy
 */
public class PropSetNotEqual extends Propagator<SetVar> {

    private final SetVar s1, s2;
    private SetDeltaMonitor s1D, s2D;

    public PropSetNotEqual(SetVar s1, SetVar s2) {
        super(new SetVar[]{s1, s2}, PropagatorPriority.LINEAR, false);
        this.s1 = s1;
        this.s1D = s1.monitorDelta(aCause);
        this.s2 = s2;
        this.s2D = s2.monitorDelta(aCause);
    }

    private boolean isS1Var(int idx) {
        return idx == 0;
    }

    private boolean isS2Var(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
    }

    private void checkNotSame() throws ContradictionException {
        if (s1.instantiated() && s2.instantiated()) {
            if (s1.getKernelSize() == s2.getKernelSize()) {
                int i = s1.getKernelFirst();
                int j = s2.getKernelFirst();
                while (i != SetVar.END) {
                    assert j != SetVar.END;
                    if (i != j) {
                        return;
                    }
                    i = s1.getKernelNext();
                    j = s2.getKernelNext();
                }
                assert j == SetVar.END;
                contradiction(s1, "Same");
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = s1.getKernelFirst(); i != SetVar.END; i = s1.getKernelNext()) {
            if (!s2.envelopeContains(i)) {
                setPassive();
                return;
            }
        }
        for (int i = s2.getKernelFirst(); i != SetVar.END; i = s2.getKernelNext()) {
            if (!s1.envelopeContains(i)) {
                setPassive();
                return;
            }
        }
        checkNotSame();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isS1Var(idxVarInProp)) {
            s1D.freeze();
            s1D.forEach(onS1Env, EventType.REMOVE_FROM_ENVELOPE);
            s1D.forEach(onS1Ker, EventType.ADD_TO_KER);
            s1D.unfreeze();
        } else {
            assert isS2Var(idxVarInProp);
            s2D.freeze();
            s2D.forEach(onS2Env, EventType.REMOVE_FROM_ENVELOPE);
            s2D.forEach(onS2Ker, EventType.ADD_TO_KER);
            s2D.unfreeze();
        }
        checkNotSame();
    }
    private final IntProcedure onS1Env = new IntProcedure() {
        @Override
        public void execute(int s1Env) throws ContradictionException {
            if (isActive() && s2.kernelContains(s1Env)) {
                setPassive();
            }
        }
    };
    private final IntProcedure onS1Ker = new IntProcedure() {
        @Override
        public void execute(int s1Ker) throws ContradictionException {
            if (isActive() && !s2.envelopeContains(s1Ker)) {
                setPassive();
            }
        }
    };
    private final IntProcedure onS2Env = new IntProcedure() {
        @Override
        public void execute(int s2Env) throws ContradictionException {
            if (isActive() && s1.kernelContains(s2Env)) {
                setPassive();
            }
        }
    };
    private final IntProcedure onS2Ker = new IntProcedure() {
        @Override
        public void execute(int s2Ker) throws ContradictionException {
            if (isActive() && !s1.envelopeContains(s2Ker)) {
                setPassive();
            }
        }
    };

    @Override
    public ESat isEntailed() {
        if (!PropUtil.isKerSubsetEnv(s1, s2) || !PropUtil.isKerSubsetEnv(s2, s1)) {
            return ESat.TRUE;
        }
        return s1.instantiated() && s2.instantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return s1 + " != " + s2;
    }
}
