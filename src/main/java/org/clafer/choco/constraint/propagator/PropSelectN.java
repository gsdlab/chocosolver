package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.Model;
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
        init[0] = n;
        System.arraycopy(bools, 0, init, 1, bools.length);
        return init;
    }

    private boolean isBoolsVar(int varIdx) {
        return varIdx > 0;
    }

    private int getBoolVarIndex(int varIdx) {
        return varIdx - 1;
    }

    private boolean isNVar(int varIdx) {
        return varIdx == 0;
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
            int index = getBoolVarIndex(idxVarInProp);
            assert bools[index].isInstantiated();
            if (bools[index].getValue() == 0) {
                for (int i = index + 1; i < bools.length; i++) {
                    bools[i].setToFalse(this);
                }
                if (n.updateUpperBound(index, this) && n.getUB() < index) {
                    for (int i = n.getUB(); i <= index; i++) {
                        bools[i].setToFalse(this);
                    }
                }
            } else {
                for (int i = 0; i < index; i++) {
                    bools[i].setToTrue(this);
                }
                if (n.updateLowerBound(index + 1, this) && n.getLB() > index + 1) {
                    for (int i = index; i < n.getLB(); i++) {
                        bools[i].setToTrue(this);
                    }
                }
            }
        } else {
            assert isNVar(idxVarInProp);
            if (IntEventType.isInclow(mask) || IntEventType.isInstantiate(mask)) {
                for (int i = 0; i < n.getLB(); i++) {
                    bools[i].setToTrue(this);
                }
            }
            if (IntEventType.isDecupp(mask) || IntEventType.isInstantiate(mask)) {
                for (int i = n.getUB(); i < bools.length; i++) {
                    bools[i].setToFalse(this);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        int leftMostZero = bools.length;
        int rightMostOne = -1;
        for (int i = 0; i < bools.length; i++) {
            if (bools[i].isInstantiated()) {
                if (bools[i].getValue() == 0) {
                    if (leftMostZero == bools.length) {
                        leftMostZero = i;
                    }
                    if (i < n.getLB()) {
                        return ESat.FALSE;
                    }
                }
                if (bools[i].getValue() == 1) {
                    rightMostOne = i;
                    if (i >= n.getUB()) {
                        return ESat.FALSE;
                    }
                }
            } else {
                allInstantiated = false;
            }
        }
        if (n.previousValue(leftMostZero + 1) < 0) {
            return ESat.FALSE;
        }
        if (n.nextValue(rightMostOne) > leftMostZero) {
            return ESat.FALSE;
        }
        return allInstantiated && n.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "selectN(" + Arrays.toString(bools) + ", " + n + ")";
    }
}
