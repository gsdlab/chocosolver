package org.clafer.choco.constraint.propagator;

import org.chocosolver.memory.IStateInt;
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
    protected int getPropagationConditions(int vIdx) {
        if (isFromVar(vIdx)) {
            return IntEventType.DECUPP.getMask();
        }
        if (isToVar(vIdx)) {
            return IntEventType.INCLOW.getMask();
        }
        assert isSetVar(vIdx);
        return SetEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int f = from.getUB();
        to.updateUpperBound(Math.max(f, PropUtil.maxEnv(set) + 1), aCause);
        int t = to.getLB();
        for (int i = f; i < t; i++) {
            set.addToKernel(i, aCause);
        }
    }

    @Override
    public ESat isEntailed() {
        if (to.getUB() - from.getLB() < set.getKernelSize()) {
            return ESat.FALSE;
        }
        int f = from.getUB();
        int t = to.getLB();
        if (t - f > set.getEnvelopeSize()) {
            return ESat.FALSE;
        }
        for (int i = f; i < t; i++) {
            if (!set.envelopeContains(i)) {
                return ESat.FALSE;
            }
        }
        return to.isInstantiated() && from.isInstantiated() && set.isInstantiated()
                ? ESat.TRUE : ESat.UNDEFINED;
    }
}
