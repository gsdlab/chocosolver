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
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

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
        this.setD = set.monitorDelta(this);
        this.bound = bound;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isBoundVar(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.ADD_TO_KER.getMask();
        }
        assert isBoundVar(vIdx);
        return IntEventType.INCLOW.getMask() + IntEventType.instantiation();
    }

    private void boundEnv() throws ContradictionException {
        int lb = bound.getLB();
        int ub = bound.getUB();
        ISetIterator iter = set.getUB().iterator();
        int i;
        while (iter.hasNext()) {
            i = iter.nextInt();
            if (i < lb) {
                set.remove(i, this);
            } else {
                if (i >= ub) {
                    // The elements in the set's envelope are at least ub.
                    setPassive();
                }
                break;
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (set.getLB().size() > 0) {
            bound.updateUpperBound(set.getLB().min(), this);
        }
        boundEnv();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            setD.freeze();
            setD.forEach(ker -> bound.updateUpperBound(ker, this), SetEventType.ADD_TO_KER);
            setD.unfreeze();
        } else {
            assert isBoundVar(idxVarInProp);
            boundEnv();
        }
    }

    @Override
    public ESat isEntailed() {
        if(!set.getLB().isEmpty() && set.getLB().min() < bound.getLB()) {
            return ESat.FALSE;
        }
        if(!set.getUB().isEmpty() && set.getUB().min() < bound.getUB()) {
            return ESat.UNDEFINED;
        }
        return ESat.TRUE;
    }

    @Override
    public String toString() {
        return set + ">>>=" + bound;
    }
}
