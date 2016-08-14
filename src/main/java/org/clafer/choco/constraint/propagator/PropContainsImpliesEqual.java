package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.memory.IStateBool;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 * if z in cond then x = y else x = {}
 *
 * @author jimmy
 */
public class PropContainsImpliesEqual extends Propagator<SetVar> {

    private final SetVar cond, x, y;
    private final ISetDeltaMonitor xD, yD;
    private final int z;
    private final IStateBool condDecided;

    public PropContainsImpliesEqual(SetVar cond, int z, SetVar x, SetVar y) {
        super(new SetVar[]{cond, x, y}, PropagatorPriority.UNARY, true);
        this.cond = cond;
        this.x = x;
        this.xD = x.monitorDelta(this);
        this.y = y;
        this.yD = y.monitorDelta(this);
        this.z = z;
        this.condDecided = cond.getEnvironment().makeBool(false);
    }

    private boolean isCondVar(int vIdx) {
        return vIdx == 0;
    }

    private boolean isXVar(int vIdx) {
        return vIdx == 1;
    }

    private boolean isYVar(int vIdx) {
        return vIdx == 2;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        PropUtil.envSubsetEnv(x, y, this);
        if (cond.getLB().contains(z) || x.getLB().size() > 0) {
            cond.force(z, this);
            PropUtil.kerSubsetKer(x, y, this);
            PropUtil.envSubsetEnv(y, x, this);
            PropUtil.kerSubsetKer(y, x, this);
            condDecided.set(true);
        } else if (!cond.getUB().contains(z) || !PropUtil.isKerSubsetEnv(y, x)) {
            cond.remove(z, this);
            x.instantiateTo(new int[]{}, this);
            condDecided.set(true);
            setPassive();
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isCondVar(idxVarInProp) || (x.getLB().size() > 0 && cond.force(z, this))) {
            if (!condDecided.get()) {
                if (cond.getLB().contains(z)) {
                    PropUtil.kerSubsetKer(x, y, this);
                    PropUtil.envSubsetEnv(y, x, this);
                    PropUtil.kerSubsetKer(y, x, this);
                    condDecided.set(true);
                } else if (!cond.getUB().contains(z)) {
                    x.instantiateTo(new int[]{}, this);
                    condDecided.set(true);
                    setPassive();
                }
            }
        } else if (isXVar(idxVarInProp)) {
            xD.freeze();
            if (cond.getLB().contains(z)) {
                xD.forEach(xKer -> y.force(xKer, this), SetEventType.ADD_TO_KER);
                xD.forEach(xEnv -> y.remove(xEnv, this), SetEventType.REMOVE_FROM_ENVELOPE);
            } else {
                xD.forEach(xEnv -> {
                    if (y.getLB().contains(xEnv)) {
                        cond.remove(z, this);
                        x.instantiateTo(new int[]{}, this);
                        condDecided.set(true);
                    }
                }, SetEventType.REMOVE_FROM_ENVELOPE);
                if (condDecided.get()) {
                    setPassive();
                }
            }
            xD.unfreeze();
        } else {
            yD.freeze();
            if (cond.getLB().contains(z)) {
                yD.forEach(yKer -> x.force(yKer, this), SetEventType.ADD_TO_KER);
            } else {
                yD.forEach(yKer -> {
                    if (!x.getUB().contains(yKer)) {
                        cond.remove(z, this);
                        x.instantiateTo(new int[]{}, this);
                        condDecided.set(true);
                    }
                }, SetEventType.ADD_TO_KER);
                if (condDecided.get()) {
                    setPassive();
                }
            }
            yD.forEach(yEnv -> x.remove(yEnv, this), SetEventType.REMOVE_FROM_ENVELOPE);
            yD.unfreeze();
        }
    }

    @Override
    public ESat isEntailed() {
        if (cond.getLB().contains(z) || x.getLB().size() > 0) {
            if (!cond.getUB().contains(z) || !PropUtil.isKerSubsetEnv(x, y) || !PropUtil.isKerSubsetEnv(y, x)) {
                return ESat.FALSE;
            }
            if (cond.getLB().contains(z) && x.isInstantiated() && y.isInstantiated()) {
                assert (Arrays.equals(x.getLB().toArray(), y.getLB().toArray()));
            }
            return cond.getLB().contains(z) && x.isInstantiated() && y.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
        } else if (!cond.getUB().contains(z)) {
            if (x.getLB().size() > 0) {
                return ESat.FALSE;
            }
            return x.getUB().size() == 0 ? ESat.TRUE : ESat.UNDEFINED;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "if " + z + " in " + cond + " { " + x + " = " + y + " } else { " + x + " = {}}";
    }
}
