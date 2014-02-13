package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.clafer.common.Util;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropLength extends Propagator<IntVar> {

    private final IntVar[] chars;

    private final IntVar length;

    public PropLength(IntVar[] chars, IntVar length) {
        super(Util.cons(length, chars), PropagatorPriority.LINEAR, true);
        this.chars = chars;
        this.length = length;
    }

    private boolean isLengthVar(int idx) {
        return idx == 0;
    }

    private boolean isCharVar(int idx) {
        return idx > 0;
    }

    private int getCharVarIndex(int idx) {
        return idx - 1;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        if (isLengthVar(vIdx)) {
            return EventType.BOUND.mask + EventType.INSTANTIATE.mask;
        }
        assert isCharVar(vIdx);
        return EventType.INT_ALL_MASK();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        length.updateUpperBound(chars.length, aCause);
        for (int i = chars.length - 1; i >= 0; i--) {
            onCharRemove(i);
        }
        for (int i = 0; i < chars.length; i++) {
            onCharInstantiate(i);
        }
        onLengthLB();
        onLengthUB();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isLengthVar(idxVarInProp)) {
            assert EventType.isInclow(mask) || EventType.isDecupp(mask);
            if (EventType.isInclow(mask)) {
                onLengthLB();
            }
            if (EventType.isDecupp(mask)) {
                onLengthUB();
            }
        } else {
            assert isCharVar(idxVarInProp);
            int id = getCharVarIndex(idxVarInProp);
            if (onCharRemove(id)) {
                onLengthLB();
            }
            if (onCharInstantiate(id)) {
                onLengthUB();
            }
        }
    }

    private void onLengthLB() throws ContradictionException {
        for (int i = 0; i < length.getLB(); i++) {
            chars[i].removeValue(0, aCause);
        }
    }

    private void onLengthUB() throws ContradictionException {
        for (int i = length.getUB(); i < chars.length; i++) {
            chars[i].instantiateTo(0, aCause);
        }
    }

    private boolean onCharRemove(int i) throws ContradictionException {
        return !chars[i].contains(0)
                && length.updateLowerBound(i + 1, aCause);
    }

    private boolean onCharInstantiate(int i) throws ContradictionException {
        return chars[i].isInstantiatedTo(0)
                && length.updateUpperBound(i, aCause);
    }

    @Override
    public ESat isEntailed() {
        if (length.getLB() > chars.length) {
            return ESat.FALSE;
        }
        for (int i = 0; i < length.getLB(); i++) {
            if (chars[i].isInstantiatedTo(0)) {
                return ESat.FALSE;
            }
        }
        for (int i = length.getUB(); i < chars.length; i++) {
            if (!chars[i].contains(0)) {
                return ESat.FALSE;
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "length(" + Arrays.toString(chars) + ", " + length + ")";
    }
}
