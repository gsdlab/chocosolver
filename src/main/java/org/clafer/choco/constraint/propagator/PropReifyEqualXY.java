package org.clafer.choco.constraint.propagator;

import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.BoolVar;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.delta.IIntDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * (reify = reifyC) <=> (x = y)
 *
 * @author jimmy
 */
public class PropReifyEqualXY extends Propagator<IntVar> {

    private final BoolVar reify;
    private final int reifyC;
    private final IntVar x, y;
    private final IIntDeltaMonitor xD, yD;

    public PropReifyEqualXY(BoolVar reify, boolean reifyC, IntVar x, IntVar y) {
        super(new IntVar[]{reify, x, y}, PropagatorPriority.BINARY, true);
        this.reify = reify;
        this.reifyC = reifyC ? 1 : 0;
        this.x = x;
        this.xD = x.monitorDelta(aCause);
        this.y = y;
        this.yD = y.monitorDelta(aCause);
    }

    private boolean isReifyVar(int idx) {
        return idx == 0;
    }

    private boolean isXVar(int idx) {
        return idx == 1;
    }

    private boolean isYVar(int idx) {
        return idx == 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.INT_ALL_MASK();
    }
    private final IntProcedure pruneXOnYRem = new IntProcedure() {
        @Override
        public void execute(int i) throws ContradictionException {
            x.removeValue(i, aCause);
        }
    };
    private final IntProcedure pruneYOnXRem = new IntProcedure() {
        @Override
        public void execute(int i) throws ContradictionException {
            y.removeValue(i, aCause);
        }
    };

    private void propagateReifyVar() throws ContradictionException {
        assert reify.instantiated();
        if (reify.getValue() == reifyC) {
            PropUtil.domSubsetDom(x, y, aCause);
            PropUtil.domSubsetDom(y, x, aCause);
            if (x.instantiated()) {
                assert y.instantiated();
                setPassive();
            }
        } else {
            if (x.instantiated()) {
                y.removeValue(x.getValue(), aCause);
                setPassive();
            } else if (y.instantiated()) {
                x.removeValue(y.getValue(), aCause);
                setPassive();
            }
        }
    }

    private void propagateXVar() throws ContradictionException {
        if (x.instantiated()) {
            if (reify.instantiated()) {
                if (reify.getValue() == reifyC) {
                    y.instantiateTo(x.getValue(), aCause);
                } else {
                    y.removeValue(x.getValue(), aCause);
                }
                setPassive();
            } else if (y.contains(x.getValue())) {
                if (y.instantiated()) {
                    reify.instantiateTo(reifyC, aCause);
                    setPassive();
                }
            } else {
                reify.instantiateTo(1 - reifyC, aCause);
                setPassive();
            }
        } else if (reify.instantiatedTo(reifyC)) {
            xD.freeze();
            xD.forEach(pruneYOnXRem, EventType.REMOVE);
            xD.unfreeze();
        }
    }

    private void propagateYVar() throws ContradictionException {
        if (y.instantiated()) {
            if (reify.instantiated()) {
                if (reify.getValue() == reifyC) {
                    x.instantiateTo(y.getValue(), aCause);
                } else {
                    x.removeValue(y.getValue(), aCause);
                }
                setPassive();
            } else if (x.contains(y.getValue())) {
                if (x.instantiated()) {
                    reify.instantiateTo(reifyC, aCause);
                    setPassive();
                }
            } else {
                reify.instantiateTo(1 - reifyC, aCause);
                setPassive();
            }
        } else if (reify.instantiatedTo(reifyC)) {
            yD.freeze();
            yD.forEach(pruneXOnYRem, EventType.REMOVE);
            yD.unfreeze();
        }
    }

    private void propagateXYVar() throws ContradictionException {
        if (!PropUtil.isDomIntersectDom(x, y)) {
            reify.instantiateTo(1 - reifyC, aCause);
            setPassive();
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (reify.instantiated()) {
            propagateReifyVar();
        } else {
            propagateXVar();
            if (!isPassive()) {
                propagateYVar();
            }
            if (!isPassive()) {
                propagateXYVar();
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isReifyVar(idxVarInProp)) {
            propagateReifyVar();
        } else if (isXVar(idxVarInProp)) {
            propagateXVar();
            if (!isPassive()) {
                propagateXYVar();
            }
        } else {
            assert isYVar(idxVarInProp);
            propagateYVar();
            if (!isPassive()) {
                propagateXYVar();
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (reify.instantiated()) {
            if (!PropUtil.isDomIntersectDom(x, y)) {
                return reify.getValue() == reifyC ? ESat.FALSE : ESat.TRUE;
            }
            if (x.instantiated() && y.instantiated()) {
                return reify.getValue() == reifyC ? ESat.TRUE : ESat.FALSE;
            }
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return (reifyC == 1 ? reify : "!" + reify) + " <=> (" + x + " = " + y + ")";
    }
}
