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
 * if i = filter then s = {} else s = {i}
 *
 * @author jimmy
 */
public class PropSingletonFilter extends Propagator<Variable> {

    private final IntVar i;
    private final IIntDeltaMonitor iD;
    private final SetVar s;
    private final ISetDeltaMonitor sD;
    private final int filter;

    public PropSingletonFilter(IntVar ivar, SetVar svar, int filter) {
        super(new Variable[]{ivar, svar}, PropagatorPriority.UNARY, true);
        this.i = ivar;
        this.iD = i.monitorDelta(this);
        this.s = svar;
        this.sD = s.monitorDelta(this);
        this.filter = filter;
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
        s.removeFromEnvelope(filter, this);
        if (s.getKernelSize() > 1) {
            contradiction(s, "Singleton cannot have more than 1 element");
        } else if (s.getKernelSize() == 1) {
            int val = s.getKernelFirst();
            i.instantiateTo(val, this);
            s.instantiateTo(new int[]{val}, this);
        } else {
            int ub = i.getUB();
            for (int val = i.getLB(); val <= ub; val = i.nextValue(val)) {
                if (val != filter && !s.envelopeContains(val)) {
                    i.removeValue(val, this);
                }
            }
            PropUtil.envSubsetDom(s, i, this);
            if (i.isInstantiated()) {
                int val = i.getValue();
                s.instantiateTo(val == filter ? new int[]{} : new int[]{val}, this);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isIVar(idxVarInProp)) {
            if (i.isInstantiated()) {
                int val = i.getValue();
                s.instantiateTo(val == filter ? new int[]{} : new int[]{val}, this);
            } else {
                iD.freeze();
                iD.forEachRemVal((IntProcedure) rem -> s.removeFromEnvelope(rem, this));
                iD.unfreeze();
            }
        } else {
            assert isSVar(idxVarInProp);
            int sKerSize = s.getKernelSize();
            if (sKerSize > 1) {
                contradiction(s, "Singleton cannot have more than 1 element");
            } else if (sKerSize == 1) {
                int val = s.getKernelFirst();
                i.instantiateTo(val, this);
                s.instantiateTo(new int[]{val}, this);
            } else {
                sD.freeze();
                sD.forEach((IntProcedure) env -> {
                    if (env != filter) {
                        i.removeValue(env, this);
                    }
                }, SetEventType.REMOVE_FROM_ENVELOPE
                );
                sD.unfreeze();
                if (i.isInstantiated()) {
                    int val = i.getValue();
                    s.instantiateTo(val == filter ? new int[]{} : new int[]{val}, this);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (s.getKernelSize() > 1) {
            return ESat.FALSE;
        }
        if (i.contains(filter)) {
            if (i.isInstantiated()) {
                if (s.getEnvelopeSize() == 0) {
                    return ESat.TRUE;
                }
                if (s.getKernelSize() > 0) {
                    return ESat.FALSE;
                }
            }
            return ESat.UNDEFINED;
        } else if (PropUtil.isDomIntersectEnv(i, s)) {
            return i.isInstantiated() && s.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
        }
        return ESat.FALSE;
    }

    @Override
    public String toString() {
        return "singletonFilter" + filter + "({" + i + "} = " + s + ")";
    }
}
