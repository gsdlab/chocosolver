package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 * The first n booleans are the true, the rest are false.
 *
 * @author jimmy
 */
public class PropSelectN extends Propagator<IntVar> {

    private final BoolVar[] bools;
    private final IntVar n;

    public PropSelectN(BoolVar[] bools, IntVar n) {
        super(buildArray(bools, n), PropagatorPriority.BINARY, true);
        this.bools = bools;
        this.n = n;
    }

    private static IntVar[] buildArray(BoolVar[] bools, IntVar n) {
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
            return IntEventType.instantiation();
        }
        assert isNVar(vIdx);
        return IntEventType.boundAndInst();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        n.updateLowerBound(0, aCause);
        n.updateUpperBound(bools.length, aCause);
        // Prune n
        for (int i = n.getLB(); i < n.getUB(); i++) {
            if (bools[i].isInstantiated()) {
                if (bools[i].getValue() == 0) {
                    n.updateUpperBound(i, aCause);
                    break;
                }
            }
        }
        for (int i = n.getUB() - 1; i >= n.getLB(); i--) {
            if (bools[i].isInstantiated()) {
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
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isBoolsVar(idxVarInProp)) {
            assert bools[idxVarInProp].isInstantiated();
            if (bools[idxVarInProp].getValue() == 0) {
                for (int i = idxVarInProp + 1; i < bools.length; i++) {
                    bools[i].setToFalse(aCause);
                }
                if (n.updateUpperBound(idxVarInProp, aCause) && n.getUB() < idxVarInProp) {
                    for (int i = n.getUB(); i <= idxVarInProp; i++) {
                        bools[i].setToFalse(aCause);
                    }
                }
            } else {
                for (int i = 0; i < idxVarInProp; i++) {
                    bools[i].setToTrue(aCause);
                }
                if (n.updateLowerBound(idxVarInProp + 1, aCause) && n.getLB() > idxVarInProp + 1) {
                    for (int i = idxVarInProp; i < n.getLB(); i++) {
                        bools[i].setToTrue(aCause);
                    }
                }
            }
        } else {
            assert isNVar(idxVarInProp);
            if (IntEventType.isInclow(mask)) {
                for (int i = 0; i < n.getLB(); i++) {
                    bools[i].setToTrue(aCause);
                }
            }
            if (IntEventType.isDecupp(mask)) {
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
            if (bools[i].isInstantiated()) {
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
        if (n.getLB() > bools.length || n.getUB() < 0) {
            return ESat.FALSE;
        }
        return allInstantiated && n.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "selectN(" + Arrays.toString(bools) + ", " + n + ")";
    }
}
