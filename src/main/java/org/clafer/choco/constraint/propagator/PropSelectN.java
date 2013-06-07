package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.BoolVar;
import solver.variables.EventType;
import solver.variables.IntVar;
import util.ESat;

/**
 * The first n booleans are the true, the rest are false.
 * 
 * @author jimmy
 */
public class PropSelectN extends Propagator<IntVar> {

    private final BoolVar[] bools;
    private final IntVar n;

    public PropSelectN(BoolVar[] bools, IntVar n) {
        super(init(bools, n), PropagatorPriority.LINEAR, true);
        this.bools = bools;
        this.n = n;
    }

    private static IntVar[] init(BoolVar[] bools, IntVar n) {
        IntVar[] init = new IntVar[bools.length + 1];
        System.arraycopy(bools, 0, init, 0, bools.length);
        init[bools.length] = n;
        return init;
    }

    private boolean isBoolsVar(int varIdx) {
        return varIdx < bools.length;
    }

    private boolean isNVar(int varIdx) {
        return varIdx == bools.length;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isBoolsVar(vIdx)) {
            return EventType.INSTANTIATE.mask;
        }
        return EventType.DECUPP.mask + EventType.INCLOW.mask + EventType.INSTANTIATE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune n
        for (int i = n.getLB(); i < n.getUB(); i++) {
            if (bools[i].instantiated()) {
                if (bools[i].getValue() == 0) {
                    n.updateUpperBound(i, aCause);
                    break;
                }
            }
        }
        for (int i = n.getUB() - 1; i >= n.getLB(); i--) {
            if (bools[i].instantiated()) {
                if (bools[i].getValue() == 1) {
                    n.updateLowerBound(i + 1, aCause);
                }
            }
        }
        // Pick bool
        for (int i = 0; i < n.getLB(); i++) {
            bools[i].setToTrue(aCause);
        }
        for (int i = n.getUB(); i < bools.length; i++) {
            bools[i].setToFalse(aCause);
        }
        assert !ESat.FALSE.equals(isEntailed());
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isBoolsVar(idxVarInProp)) {
            assert bools[idxVarInProp].instantiated();
            if (bools[idxVarInProp].getValue() == 0) {
                for (int i = idxVarInProp + 1; i < bools.length; i++) {
                    bools[i].setToFalse(aCause);
                }
                n.updateUpperBound(idxVarInProp, aCause);
            } else {
                for (int i = 0; i < idxVarInProp; i++) {
                    bools[i].setToTrue(aCause);
                }
                n.updateLowerBound(idxVarInProp + 1, aCause);
            }
        } else {
            assert isNVar(idxVarInProp);
            if (EventType.isInclow(mask)) {
                for (int i = 0; i < n.getLB(); i++) {
                    bools[i].setToTrue(aCause);
                }
            }
            if (EventType.isDecupp(mask)) {
                for (int i = n.getUB(); i < bools.length; i++) {
                    bools[i].setToFalse(aCause);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        for (int i = 0; i < bools.length; i++) {
            if (bools[i].instantiated()) {
                if (bools[i].getValue() == 0 && i < n.getLB()) {
                    return ESat.FALSE;
                }
                if (bools[i].getValue() == 1 && i >= n.getUB()) {
                    return ESat.FALSE;
                }
            } else {
                allInstantiated = false;
            }
        }
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "selectN(" + Arrays.toString(bools) + ", " + n + ")";
    }
}
