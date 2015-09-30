package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.util.ESat;

/**
 * {@code (value = array[index + offset]) || (value = support)}
 *
 * @author jimmy
 */
public class PropElementValueSupport extends Propagator<IntVar> {

    private final IntVar value;
    private final IntVar[] array;
    private final IntVar index;
    private final int offset;
    private final int support;

    public PropElementValueSupport(IntVar value, IntVar[] array, IntVar index, int offset, int support) {
        super(buildArray(value, array, index), PropagatorPriority.LINEAR, false);
        this.value = value;
        this.array = array;
        this.index = index;
        this.offset = offset;
        this.support = support;
    }

    private static IntVar[] buildArray(IntVar value, IntVar[] array, IntVar index) {
        IntVar[] variables = new IntVar[array.length + 2];
        variables[0] = value;
        variables[1] = index;
        System.arraycopy(array, 0, variables, 2, array.length);
        return variables;
    }

    private boolean supportForValue(int value) {
        if (value == support) {
            return true;
        }
        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            int j = i + offset;
            if (j >= 0 && j < array.length && array[j].contains(value)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        index.updateLowerBound(-offset, this);
        index.updateUpperBound(array.length - offset - 1, this);

        if (!value.contains(support)) {
            int ub = index.getUB();
            for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
                if (!PropUtil.isDomIntersectDom(value, array[i + offset])) {
                    index.removeValue(i, this);
                }
            }
        }

        if (index.isInstantiated() && !value.contains(support)) {
            PropUtil.domSubsetDom(value, array[index.getValue() + offset], this);
            PropUtil.domSubsetDom(array[index.getValue() + offset], value, this);
            if (value.isInstantiated()) {
                setPassive();
            }
        } else {
            int ub = value.getUB();
            for (int i = value.getLB(); i <= ub; i = value.nextValue(i)) {
                if (!supportForValue(i)) {
                    value.removeValue(i, this);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        int lb = index.getLB();
        int ub = index.getUB();
        if (lb + offset >= array.length || ub + offset < 0) {
            return ESat.FALSE;
        }
        if (lb + offset < 0 || ub + offset >= array.length) {
            return ESat.UNDEFINED;
        }
        if (value.isInstantiatedTo(support)) {
            return ESat.TRUE;
        }
        if (index.isInstantiated() && value.isInstantiated() && array[lb + offset].isInstantiatedTo(value.getValue())) {
            return ESat.TRUE;
        }
        if (value.contains(support)) {
            return ESat.UNDEFINED;
        }
        for (int i = lb; i <= ub; i = index.nextValue(i)) {
            int j = i + offset;
            if (j >= 0 && j < array.length && PropUtil.isDomIntersectDom(value, array[j])) {
                return ESat.UNDEFINED;
            }
        }
        return ESat.FALSE;
    }

    @Override
    public String toString() {
        return "elementValueSupport " + value + " = " + Arrays.toString(array) + "[" + index + " + " + offset + "] || " + support;
    }
}
