package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 * (reify = reifyC) <=> (x = c)
 *
 * @author jimmy
 */
public class PropReifyEqualXC extends Propagator<IntVar> {

    private final IntVar reify;
    private final int reifyC;
    private final IntVar x;
    private final int c;

    public PropReifyEqualXC(BoolVar reify, boolean reifyC, IntVar x, int c) {
        this(reify, reifyC ? 1 : 0, x, c);
    }

    public PropReifyEqualXC(IntVar reify, int reifyC, IntVar x, int c) {
        super(new IntVar[]{reify, x}, PropagatorPriority.UNARY, true);
        this.reify = reify;
        this.reifyC = reifyC;
        this.x = x;
        this.c = c;
    }

    private boolean isReifyVar(int idx) {
        return idx == 0;
    }

    private boolean isXVar(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return IntEventType.all();
    }

    private void propagateReifyVar() throws ContradictionException {
        assert reify.isInstantiated();
        if (reify.getValue() == reifyC) {
            x.instantiateTo(c, aCause);
        } else {
            x.removeValue(c, aCause);
        }
        setPassive();
    }

    private void propagateXVar() throws ContradictionException {
        if (x.contains(c)) {
            if (x.isInstantiated()) {
                reify.instantiateTo(reifyC, aCause);
                setPassive();
            }
        } else {
            reify.instantiateTo(1 - reifyC, aCause);
            setPassive();
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (reify.isInstantiated()) {
            propagateReifyVar();
        } else {
            propagateXVar();
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isReifyVar(idxVarInProp)) {
            propagateReifyVar();
        } else {
            assert isXVar(idxVarInProp);
            propagateXVar();
        }
    }

    @Override
    public ESat isEntailed() {
        if (reify.isInstantiated()) {
            if (!x.contains(c)) {
                return reify.getValue() == reifyC ? ESat.FALSE : ESat.TRUE;
            }
            if (x.isInstantiated()) {
                return reify.getValue() == reifyC ? ESat.TRUE : ESat.FALSE;
            }
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return (reifyC == 1 ? reify : "!" + reify) + " <=> (" + x + " = " + c + ")";
    }
}
