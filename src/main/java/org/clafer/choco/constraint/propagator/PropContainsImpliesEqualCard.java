package org.clafer.choco.constraint.propagator;

import org.chocosolver.memory.IStateBool;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.IIntDeltaMonitor;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 * if z in cond then x = y else x = 0
 *
 * @author jimmy
 */
public class PropContainsImpliesEqualCard extends Propagator<Variable> {

    private final SetVar cond;
    private final IntVar x, y;
    private final IIntDeltaMonitor xD, yD;
    private final int z;
    // Support that x intersects y.
    private int xSupportY = 0;
    private final IStateBool condDecided;

    public PropContainsImpliesEqualCard(SetVar cond, int z, IntVar x, IntVar y) {
        super(new Variable[]{cond, x, y}, PropagatorPriority.UNARY, true);
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

//    @Override
//    public int getPropagationConditions(int vIdx) {
//        if (vIdx == 0) {
//            return SetEventType.all();
//        }
//        if (vIdx == 1) {
//            return IntEventType.INSTANTIATE.getMask() + IntEventType.INCLOW.getMask();
//        }
//        assert vIdx == 2;
//        return IntEventType.boundAndInst();
//    }
    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int ub = x.getUB();
        for (int i = x.getLB(); i <= ub; i = x.nextValue(i)) {
            if (i != 0 && !y.contains(i)) {
                x.removeValue(i, this);
            }
        }
        if (!cond.getUB().contains(z) || y.isInstantiatedTo(0)) {
            x.instantiateTo(0, this);
            condDecided.set(true);
            setPassive();
        } else if (cond.getLB().contains(z) || !x.contains(0)) {
            cond.force(z, this);
            PropUtil.domSubsetDom(x, y, this);
            PropUtil.domSubsetDom(y, x, this);
            condDecided.set(true);
        } else if (x.isInstantiatedTo(0)) {
            if (!y.contains(0)) {
                cond.remove(z, this);
                condDecided.set(true);
                setPassive();
            }
        }
    }

    private boolean xIntersectsY() {
        if (x.contains(xSupportY) && y.contains(xSupportY)) {
            return true;
        }
        xSupportY = PropUtil.getDomIntersectDom(x, x);
        return xSupportY != Integer.MAX_VALUE;
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isCondVar(idxVarInProp) || (!x.contains(0) && cond.force(z, this))) {
            if (!condDecided.get()) {
                if (cond.getLB().contains(z)) {
                    PropUtil.domSubsetDom(x, y, this);
                    PropUtil.domSubsetDom(y, x, this);
                    condDecided.set(true);
                } else if (!cond.getUB().contains(z)) {
                    x.instantiateTo(0, this);
                    condDecided.set(true);
                    setPassive();
                }
            }
        } else if (isXVar(idxVarInProp)) {
            xD.freeze();
            if (!xIntersectsY()) {
                cond.remove(z, this);
                x.instantiateTo(0, this);
                condDecided.set(true);
                setPassive();
            } else if (cond.getLB().contains(z)) {
                xD.forEachRemVal((IntProcedure) xRem -> y.removeValue(xRem, this));
            }
            xD.unfreeze();
            propagate(mask);
        } else {
            assert isYVar(idxVarInProp);
            yD.freeze();
            if (cond.getLB().contains(z)) {
                yD.forEachRemVal((IntProcedure) yRem -> x.removeValue(yRem, this));
            } else {
                yD.forEachRemVal((IntProcedure) yRem -> {
                    if (yRem != 0) {
                        x.removeValue(yRem, this);
                    }
                });
            }
            yD.unfreeze();
            if (x.isInstantiatedTo(0) && !y.contains(0)) {
                cond.remove(z, this);
                condDecided.set(true);
                setPassive();
            }
        }
    }

    @Override
    public ESat isEntailed() {
        return ESat.TRUE;
    }
}
