package org.clafer.constraint.propagator;

import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * More efficient than the provided PropAllEqual.
 * 
 * @author jimmy
 */
public class PropSetEqual extends Propagator<SetVar> {

    private final SetVar s1, s2;
    private SetDeltaMonitor s1D, s2D;

    public PropSetEqual(SetVar s1, SetVar s2) {
        super(new SetVar[]{s1, s2}, PropagatorPriority.LINEAR);
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

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        PropUtil.envSubsetEnv(s1, s2, aCause);
        PropUtil.envSubsetEnv(s2, s1, aCause);
        PropUtil.kerSubsetKer(s1, s2, aCause);
        PropUtil.kerSubsetKer(s2, s1, aCause);
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isS1Var(idxVarInProp)) {
            s1D.freeze();
            s1D.forEach(pickS2OnS1Ker, EventType.ADD_TO_KER);
            s1D.forEach(pruneS2OnS1Env, EventType.REMOVE_FROM_ENVELOPE);
            s1D.unfreeze();
        } else {
            assert isS2Var(idxVarInProp);
            s2D.freeze();
            s2D.forEach(pickS1OnS2Ker, EventType.ADD_TO_KER);
            s2D.forEach(pruneS1OnS2Env, EventType.REMOVE_FROM_ENVELOPE);
            s2D.unfreeze();
        }
    }
    private final IntProcedure pruneS2OnS1Env = new IntProcedure() {

        @Override
        public void execute(int s1Env) throws ContradictionException {
            s2.removeFromEnvelope(s1Env, aCause);
        }
    };
    private final IntProcedure pickS2OnS1Ker = new IntProcedure() {

        @Override
        public void execute(int s1Ker) throws ContradictionException {
            s2.addToKernel(s1Ker, aCause);
        }
    };
    private final IntProcedure pruneS1OnS2Env = new IntProcedure() {

        @Override
        public void execute(int s2Env) throws ContradictionException {
            s1.removeFromEnvelope(s2Env, aCause);
        }
    };
    private final IntProcedure pickS1OnS2Ker = new IntProcedure() {

        @Override
        public void execute(int s2Ker) throws ContradictionException {
            s1.addToKernel(s2Ker, aCause);
        }
    };

    @Override
    public ESat isEntailed() {
        if (!PropUtil.isKerSubsetEnv(s1, s2) || !PropUtil.isKerSubsetEnv(s2, s1)) {
            return ESat.FALSE;
        }
        return s1.instantiated() && s2.instantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return s1 + " = " + s2;
    }
}
