package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 * More efficient than the provided PropAllEqual.
 *
 * @author jimmy
 */
public class PropSetEqual extends Propagator<SetVar> {

    private final SetVar s1, s2;
    private final ISetDeltaMonitor s1D, s2D;

    public PropSetEqual(SetVar s1, SetVar s2) {
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

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        PropUtil.envSubsetEnv(s1, s2, this);
        PropUtil.envSubsetEnv(s2, s1, this);
        PropUtil.kerSubsetKer(s1, s2, this);
        PropUtil.kerSubsetKer(s2, s1, this);
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isS1Var(idxVarInProp)) {
            s1D.freeze();
            s1D.forEach(s1Env -> s2.remove(s1Env, this), SetEventType.REMOVE_FROM_ENVELOPE);
            s1D.forEach(s1Ker -> s2.force(s1Ker, this), SetEventType.ADD_TO_KER);
            s1D.unfreeze();
        } else {
            assert isS2Var(idxVarInProp);
            s2D.freeze();
            s2D.forEach(s2Env -> s1.remove(s2Env, this), SetEventType.REMOVE_FROM_ENVELOPE);
            s2D.forEach(s2Ker -> s1.force(s2Ker, this), SetEventType.ADD_TO_KER);
            s2D.unfreeze();
        }
    }

    @Override
    public ESat isEntailed() {
        if (!PropUtil.isKerSubsetEnv(s1, s2) || !PropUtil.isKerSubsetEnv(s2, s1)) {
            return ESat.FALSE;
        }
        if (s1.isInstantiated() && s2.isInstantiated()) {
            return ESat.TRUE;
        }
        int setIntersection = 0;
        ISetIterator iter = s1.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (s2.getUB().contains(i)) {
                setIntersection++;
            }
        }
        if (setIntersection < s1.getCard().getLB() || setIntersection < s2.getCard().getLB()) {
            return ESat.FALSE;
        }

        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return s1 + " = " + s2;
    }
}
