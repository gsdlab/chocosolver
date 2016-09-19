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
        this.iD = i.monitorDelta(this);
        this.s = svar;
        this.sD = s.monitorDelta(this);
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
        if (s.getLB().size() > 1) {
            fails();
        } else if (s.getLB().size() == 1) {
            int val = s.getLB().min();
            i.instantiateTo(val, this);
            s.instantiateTo(new int[]{val}, this);
        } else {
            PropUtil.domSubsetEnv(i, s, this);
            PropUtil.envSubsetDom(s, i, this);
            if (i.isInstantiated()) {
                s.instantiateTo(new int[]{i.getValue()}, this);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isIVar(idxVarInProp)) {
            if (i.isInstantiated()) {
                s.instantiateTo(new int[]{i.getValue()}, this);
            } else {
                iD.freeze();
                iD.forEachRemVal((IntProcedure) rem -> s.remove(rem, this));
                iD.unfreeze();
            }
        } else {
            assert isSVar(idxVarInProp);
            int sKerSize = s.getLB().size();
            if (sKerSize > 1) {
                fails();
            } else if (sKerSize == 1) {
                int val = s.getLB().min();
                i.instantiateTo(val, this);
                s.instantiateTo(new int[]{val}, this);
            } else {
                sD.freeze();
                sD.forEach(env -> i.removeValue(env, this), SetEventType.REMOVE_FROM_ENVELOPE);
                sD.unfreeze();
                if (i.isInstantiated()) {
                    s.instantiateTo(new int[]{i.getValue()}, this);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        int lb = s.getLB().size();
        if (lb > 1) {
            return ESat.FALSE;
        }
        if (lb == 1) {
            if (i.contains(s.getLB().min())) {
                return i.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
            } else {
                return ESat.FALSE;
            }
        }
        if (s.getUB().size() < 1) {
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
