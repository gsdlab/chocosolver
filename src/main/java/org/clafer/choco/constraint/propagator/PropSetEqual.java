package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 * More efficient than the provided PropAllEqual.
 *
 * @author jimmy
 */
public class PropSetEqual extends Propagator<SetVar> {

    private final SetVar s1, s2;
    private ISetDeltaMonitor s1D, s2D;

    public PropSetEqual(SetVar s1, SetVar s2) {
        super(new SetVar[]{s1, s2}, PropagatorPriority.LINEAR, true);
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
        return SetEventType.all();
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
            s1D.forEach(pruneS2OnS1Env, SetEventType.REMOVE_FROM_ENVELOPE);
            s1D.forEach(pickS2OnS1Ker, SetEventType.ADD_TO_KER);
            s1D.unfreeze();
        } else {
            assert isS2Var(idxVarInProp);
            s2D.freeze();
            s2D.forEach(pruneS1OnS2Env, SetEventType.REMOVE_FROM_ENVELOPE);
            s2D.forEach(pickS1OnS2Ker, SetEventType.ADD_TO_KER);
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
        return s1.isInstantiated() && s2.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return s1 + " = " + s2;
    }
}
