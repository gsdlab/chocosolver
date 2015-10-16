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
// TODO: Sept16
//    @Override
//    public int getPropagationConditions(int vIdx) {
//        if (isBoolsVar(vIdx)) {
//            return IntEventType.instantiation();
//        }
////        assert isNVar(vIdx);
//        return IntEventType.boundAndInst();
//    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        n.updateLowerBound(0, this);
        n.updateUpperBound(bools.length, this);
        // Prune n
        for (int i = n.getLB(); i < n.getUB(); i++) {
            if (bools[i].isInstantiated()) {
                if (bools[i].getValue() == 0) {
                    n.updateUpperBound(i, this);
                    break;
                }
            }
        }
        for (int i = n.getUB() - 1; i >= n.getLB(); i--) {
            if (bools[i].isInstantiated()) {
                if (bools[i].getValue() == 1) {
                    n.updateLowerBound(i + 1, this);
                }
            }
        }
        // Pick bool
        for (int i = 0; i < n.getLB(); i++) {
            bools[i].setToTrue(this);
        }
        for (int i = n.getUB(); i < bools.length; i++) {
            bools[i].setToFalse(this);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isBoolsVar(idxVarInProp)) {
            assert bools[idxVarInProp].isInstantiated();
            if (bools[idxVarInProp].getValue() == 0) {
                for (int i = idxVarInProp + 1; i < bools.length; i++) {
                    bools[i].setToFalse(this);
                }
                if (n.updateUpperBound(idxVarInProp, this) && n.getUB() < idxVarInProp) {
                    for (int i = n.getUB(); i <= idxVarInProp; i++) {
                        bools[i].setToFalse(this);
                    }
                }
            } else {
                for (int i = 0; i < idxVarInProp; i++) {
                    bools[i].setToTrue(this);
                }
                if (n.updateLowerBound(idxVarInProp + 1, this) && n.getLB() > idxVarInProp + 1) {
                    for (int i = idxVarInProp; i < n.getLB(); i++) {
                        bools[i].setToTrue(this);
                    }
                }
            }
        } else {
            assert isNVar(idxVarInProp);
            if (IntEventType.isInclow(mask)) {
                for (int i = 0; i < n.getLB(); i++) {
                    bools[i].setToTrue(this);
                }
            }
            if (IntEventType.isDecupp(mask)) {
                for (int i = n.getUB(); i < bools.length; i++) {
                    bools[i].setToFalse(this);
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
