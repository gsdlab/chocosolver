package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;
import org.chocosolver.util.procedure.IntProcedure;

/**
 * More efficient than the provided PropAllDiff. Idempotent unlike the
 * PropAllDiff.
 *
 * @author jimmy
 */
public class PropSetNotEqual extends Propagator<SetVar> {

    private final SetVar s1, s2;
    private final ISetDeltaMonitor s1D, s2D;

    public PropSetNotEqual(SetVar s1, SetVar s2) {
        super(new SetVar[]{s1, s2}, PropagatorPriority.LINEAR, true);
        this.s1 = s1;
        this.s1D = s1.monitorDelta(this);
        this.s2 = s2;
        this.s2D = s2.monitorDelta(this);
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

    private void checkNotSame() throws ContradictionException {
        if (s1.isInstantiated() && s2.isInstantiated()) {
            if (PropUtil.equals(s1.getLB(), s2.getLB())) {
                fails();
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        ISetIterator s1Ker = s1.getLB().iterator();
        while(s1Ker.hasNext()) {
            int i = s1Ker.nextInt();
            if (!s2.getUB().contains(i)) {
                setPassive();
                return;
            }
        }
        ISetIterator s2Ker = s2.getLB().iterator();
        while(s2Ker.hasNext()) {
            int i = s2Ker.nextInt();
            if (!s1.getUB().contains(i)) {
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
            s1D.forEach(onS1Env, SetEventType.REMOVE_FROM_ENVELOPE);
            s1D.forEach(onS1Ker, SetEventType.ADD_TO_KER);
            s1D.unfreeze();
        } else {
            assert isS2Var(idxVarInProp);
            s2D.freeze();
            s2D.forEach(onS2Env, SetEventType.REMOVE_FROM_ENVELOPE);
            s2D.forEach(onS2Ker, SetEventType.ADD_TO_KER);
            s2D.unfreeze();
        }
        checkNotSame();
    }
    private final IntProcedure onS1Env = new IntProcedure() {
        @Override
        public void execute(int s1Env) throws ContradictionException {
            if (isActive() && s2.getLB().contains(s1Env)) {
                setPassive();
            }
        }
    };
    private final IntProcedure onS1Ker = new IntProcedure() {
        @Override
        public void execute(int s1Ker) throws ContradictionException {
            if (isActive() && !s2.getUB().contains(s1Ker)) {
                setPassive();
            }
        }
    };
    private final IntProcedure onS2Env = new IntProcedure() {
        @Override
        public void execute(int s2Env) throws ContradictionException {
            if (isActive() && s1.getLB().contains(s2Env)) {
                setPassive();
            }
        }
    };
    private final IntProcedure onS2Ker = new IntProcedure() {
        @Override
        public void execute(int s2Ker) throws ContradictionException {
            if (isActive() && !s1.getUB().contains(s2Ker)) {
                setPassive();
            }
        }
    };

    @Override
    public ESat isEntailed() {
        if (!PropUtil.isKerSubsetEnv(s1, s2) || !PropUtil.isKerSubsetEnv(s2, s1)) {
            return ESat.TRUE;
        }
        return s1.isInstantiated() && s2.isInstantiated() ? ESat.FALSE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return s1 + " != " + s2;
    }
}
