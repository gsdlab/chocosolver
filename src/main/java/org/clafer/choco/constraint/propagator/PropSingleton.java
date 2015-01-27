package org.clafer.choco.constraint.propagator;

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
public class PropSingleton extends Propagator<Variable> {

    private final IntVar i;
    private final IIntDeltaMonitor iD;
    private final SetVar s;
    private final ISetDeltaMonitor sD;

    public PropSingleton(IntVar ivar, SetVar svar) {
        super(new Variable[]{ivar, svar}, PropagatorPriority.UNARY, true);
        this.i = ivar;
        this.iD = i.monitorDelta(aCause);
        this.s = svar;
        this.sD = s.monitorDelta(aCause);
    }

    private boolean isIVar(int idx) {
        return idx == 0;
    }

    private boolean isSVar(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isIVar(vIdx)) {
            return IntEventType.all();
        }
        assert isSVar(vIdx);
        return SetEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (s.getKernelSize() > 1) {
            contradiction(s, "Singleton cannot have more than 1 element");
        } else if (s.getKernelSize() == 1) {
            int val = s.getKernelFirst();
            i.instantiateTo(val, aCause);
            s.instantiateTo(new int[]{val}, aCause);
        }
        PropUtil.domSubsetEnv(i, s, aCause);
        PropUtil.envSubsetDom(s, i, aCause);
        if (i.isInstantiated()) {
            s.instantiateTo(new int[]{i.getValue()}, aCause);
        } else if (s.getEnvelopeSize() == 1) {
            int val = s.getEnvelopeFirst();
            i.instantiateTo(val, aCause);
            s.instantiateTo(new int[]{val}, aCause);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isIVar(idxVarInProp)) {
            if (i.isInstantiated()) {
                s.instantiateTo(new int[]{i.getValue()}, aCause);
            } else {
                iD.freeze();
                iD.forEachRemVal(pruneSOnIRem);
                iD.unfreeze();
            }
        } else {
            assert isSVar(idxVarInProp);
            int sKerSize = s.getKernelSize();
            if (sKerSize > 1) {
                contradiction(s, "Singleton cannot have more than 1 element");
            } else if (sKerSize == 1) {
                int val = s.getKernelFirst();
                i.instantiateTo(val, aCause);
                s.instantiateTo(new int[]{val}, aCause);
            } else {
                sD.freeze();
                sD.forEach(pruneIOnSEnv, SetEventType.REMOVE_FROM_ENVELOPE);
                sD.unfreeze();
                if (i.isInstantiated()) {
                    s.instantiateTo(new int[]{i.getValue()}, aCause);
                }
            }
        }
    }
    private final IntProcedure pruneSOnIRem = new IntProcedure() {
        @Override
        public void execute(int i) throws ContradictionException {
            s.removeFromEnvelope(i, aCause);
        }
    };
    private final IntProcedure pruneIOnSEnv = new IntProcedure() {
        @Override
        public void execute(int sEnv) throws ContradictionException {
            i.removeValue(sEnv, aCause);
        }
    };

    @Override
    public ESat isEntailed() {
        if (s.getKernelSize() > 1) {
            return ESat.FALSE;
        }
        if (s.getEnvelopeSize() < 1) {
            return ESat.FALSE;
        }
        if (PropUtil.isDomIntersectEnv(i, s)) {
            return i.isInstantiated() && s.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
        }
        return ESat.FALSE;
    }

    @Override
    public String toString() {
        return "singleton({" + i + "} = " + s + ")";
    }
}
