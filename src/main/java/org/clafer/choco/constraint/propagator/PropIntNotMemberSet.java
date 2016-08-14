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
 * Missing from the library.
 *
 * @author jimmy
 */
public class PropIntNotMemberSet extends Propagator<Variable> {

    private final IntVar element;
    private final SetVar set;
    private final ISetDeltaMonitor setD;

    public PropIntNotMemberSet(IntVar element, SetVar set) {
        super(new Variable[]{element, set}, PropagatorPriority.BINARY, true);
        this.element = element;
        this.set = set;
        this.setD = set.monitorDelta(this);
    }

    private boolean isElementVar(int idx) {
        return idx == 0;
    }

    private boolean isSetVar(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isElementVar(vIdx)) {
            return IntEventType.instantiation();
        }
        assert isSetVar(vIdx);
        return SetEventType.ADD_TO_KER.getMask();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        ISetIterator iter = set.getLB().iterator();
        while (iter.hasNext()) {
            element.removeValue(iter.nextInt(), this);
        }
        if (element.isInstantiated()) {
            set.remove(element.getValue(), this);
            setPassive();
        } else if (set.isInstantiated()) {
            setPassive();
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isElementVar(idxVarInProp)) {
            assert element.isInstantiated();
            set.remove(element.getValue(), this);
            setPassive();
        } else {
            assert isSetVar(idxVarInProp);
            setD.freeze();
            setD.forEach(setKer -> element.removeValue(setKer, this), SetEventType.ADD_TO_KER);
            setD.unfreeze();
            if (element.isInstantiated()) {
                set.remove(element.getValue(), this);
                setPassive();
            } else if (set.isInstantiated()) {
                setPassive();
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (element.isInstantiated()) {
            if (!set.getUB().contains(element.getValue())) {
                return ESat.TRUE;
            }
            return set.isInstantiated() ? ESat.FALSE : ESat.UNDEFINED;
        }
        if (PropUtil.isDomSubsetKer(element, set)) {
            return ESat.FALSE;
        }
        return PropUtil.isDomIntersectEnv(element, set) ? ESat.UNDEFINED : ESat.TRUE;
    }

    @Override
    public String toString() {
        return element + " not in " + set;
    }
}
