package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import util.ESat;

/**
 * array[index] = value.
 *
 * If multiple elements have the value, the index variable is the LOWEST index.
 * For example {@code find(3, [0,3,2,3], index)} would enforce {@code index = 1}
 * since {@code array[1] = 3} and {@code array[3] = 3} but 1 is the lower index.
 *
 * @author jimmy
 */
@Deprecated
public class PropFind extends Propagator<IntVar> {

    private final int value;
    private IntVar[] array;
    private final IntVar index;

    public PropFind(int value, IntVar[] array, IntVar index) {
        super(Util.cons(index, array), PropagatorPriority.TERNARY, true);
        this.value = value;
        this.array = Check.noNulls(array);
        this.index = Check.notNull(index);
    }

    private boolean isIndexVar(int idx) {
        return idx == 0;
    }

    private boolean isArrayVar(int idx) {
        return idx > 0;
    }

    private int getArrayVarIndex(int idx) {
        assert isArrayVar(idx);
        return idx - 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isIndexVar(vIdx)) {
            return EventType.INSTANTIATE.mask + EventType.INCLOW.mask;
        }
        assert isArrayVar(vIdx);
        if (index.contains(getArrayVarIndex(vIdx))) {
            return EventType.INT_ALL_MASK();
        }
        return EventType.VOID.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            if (!array[i].contains(value)) {
                index.removeValue(i, aCause);
            }
        }
        int lb = index.getLB();
        for (int i = 0; i < lb; i++) {
            array[i].removeValue(value, aCause);
        }
        if (index.instantiated()) {
            array[index.getValue()].instantiateTo(value, aCause);
            setPassive();
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isArrayVar(idxVarInProp)) {
            int id = getArrayVarIndex(idxVarInProp);
            if (!array[id].contains(value)) {
                index.removeValue(id, aCause);
            }
        }
        int lb = index.getLB();
        for (int i = 0; i < lb; i++) {
            array[i].removeValue(value, aCause);
        }
        if (index.instantiated()) {
            array[index.getValue()].instantiateTo(value, aCause);
            setPassive();
        }
    }

    @Override
    public ESat isEntailed() {
        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            if (!array[i].contains(value)) {
                return ESat.FALSE;
            }
        }
        if (index.instantiated()) {
            for (int i = 0; i < array.length; i++) {
                if (array[i].contains(value)) {
                    return array[i].instantiated() ? ESat.eval(index.getValue() == i) : ESat.UNDEFINED;
                }
            }
            return ESat.FALSE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "find(" + value + ", " + Arrays.toString(array) + ", " + index + ")";
    }
}
