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
 * {@code i ∈ set ⇒ i < bound}
 *
 * @author jimmy
 */
public class PropSetStrictHighBound extends Propagator<Variable> {

    private final SetVar set;
    private final ISetDeltaMonitor setD;
    private final IntVar bound;

    public PropSetStrictHighBound(SetVar set, IntVar bound) {
        super(new Variable[]{set, bound}, PropagatorPriority.UNARY, true);
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
        return IntEventType.DECUPP.getMask() + IntEventType.instantiation();
    }

    private void boundEnv() throws ContradictionException {
        int lb = bound.getLB();
        int ub = bound.getUB();
        boolean smallerThanLb = true;
        ISetIterator iter = set.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (i >= ub) {
                set.remove(i, this);
            } else if (i >= lb) {
                smallerThanLb = false;
            }
        }
        if (smallerThanLb) {
            // The elements in the set's envelope are less than lb.
            setPassive();
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (set.getLB().size() > 0) {
            bound.updateLowerBound(set.getLB().max() + 1, this);
        }
        boundEnv();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            setD.freeze();
            setD.forEach(ker -> bound.updateLowerBound(ker + 1, this), SetEventType.ADD_TO_KER);
            setD.unfreeze();
        } else {
            assert isBoundVar(idxVarInProp);
            boundEnv();
        }
    }

    @Override
    public ESat isEntailed() {
        ISetIterator setKer = set.getLB().iterator();
        while (setKer.hasNext()) {
            int i = setKer.nextInt();
            if (i >= bound.getUB()) {
                return ESat.FALSE;
            }
        }
        ISetIterator setEnv = set.getUB().iterator();
        while (setEnv.hasNext()) {
            int i = setEnv.nextInt();
            if (i >= bound.getLB()) {
                return ESat.UNDEFINED;
            }
        }
        return ESat.TRUE;
    }

    @Override
    public String toString() {
        return set + "<<<" + bound;
    }
}
