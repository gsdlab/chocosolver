package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropElement extends Propagator<IntVar> {

    private final IntVar value;
    private final IntVar[] array;
    private final IntVar index;
    private final int offset;

    public PropElement(IntVar value, IntVar[] array, IntVar index, int offset) {
        super(buildArray(value, array, index), PropagatorPriority.LINEAR, false);
        this.value = value;
        this.array = array;
        this.index = index;
        this.offset = offset;
    }

    private static IntVar[] buildArray(IntVar value, IntVar[] array, IntVar index) {
        IntVar[] variables = new IntVar[array.length + 2];
        variables[0] = value;
        variables[1] = index;
        System.arraycopy(array, 0, variables, 2, array.length);
        return variables;
    }

    private boolean supportForValue(int value) {
        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            if (array[i + offset].contains(value)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        index.updateLowerBound(-offset, aCause);
        index.updateUpperBound(array.length - offset - 1, aCause);

        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            if (!PropUtil.isDomIntersectDom(value, array[i + offset])) {
                index.removeValue(i, aCause);
            }
        }
        ub = value.getUB();
        for (int i = value.getLB(); i <= ub; i = value.nextValue(i)) {
            if (!supportForValue(i)) {
                value.removeValue(i, aCause);
            }
        }
        if (index.isInstantiated()) {
            PropUtil.domSubsetDom(array[index.getValue() + offset], value, aCause);
            if (value.isInstantiated()) {
                setPassive();
            }
        }
    }

    @Override
    public ESat isEntailed() {
        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            int j = i + offset;
            if (j >= 0 && j < array.length && PropUtil.isDomIntersectDom(value, array[j])) {
                return index.isInstantiated() && value.isInstantiated() && array[j].isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
            }
        }
        return ESat.FALSE;
    }

    @Override
    public String toString() {
        return "Element {" + Arrays.toString(array) + "}[" + index + "] = " + value;
    }
}
