package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSetBounded extends Propagator<Variable> {

    private final IntVar from, to;
    private final SetVar set;

    public PropSetBounded(IntVar from, IntVar to, SetVar set) {
        super(new Variable[]{from, to, set}, PropagatorPriority.BINARY, false);
        this.from = from;
        this.to = to;
        this.set = set;
    }

    boolean isFromVar(int idx) {
        return idx == 0;
    }

    boolean isToVar(int idx) {
        return idx == 1;
    }

    boolean isSetVar(int idx) {
        return idx == 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isFromVar(vIdx)) {
            return IntEventType.INSTANTIATE.getMask() + IntEventType.DECUPP.getMask();
        }
        if (isToVar(vIdx)) {
            return IntEventType.INSTANTIATE.getMask() + IntEventType.INCLOW.getMask();
        }
        assert isSetVar(vIdx);
        return SetEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        from.updateUpperBound(to.getUB(), this);
        to.updateLowerBound(from.getLB(), this);
        if (from.getUB() < to.getLB() && !set.getUB().isEmpty()) {
            from.updateLowerBound(set.getUB().min(), this);
            to.updateUpperBound(set.getUB().max() + 1, this);
        }
        int f = from.getUB();
        int t = to.getLB();
        for (int i = f; i < t; i++) {
            set.force(i, this);
        }
    }

    @Override
    public ESat isEntailed() {
        if (from.getLB() > to.getUB()) {
            return ESat.FALSE;
        }
        int f = from.getUB();
        int t = to.getLB();
        if (t - f > set.getUB().size()) {
            return ESat.FALSE;
        }
        for (int i = f; i < t; i++) {
            if (!set.getUB().contains(i)) {
                return ESat.FALSE;
            }
        }
        return to.isInstantiated() && from.isInstantiated() && set.isInstantiated()
                ? ESat.TRUE : ESat.UNDEFINED;
    }
}
