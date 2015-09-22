package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.util.ESat;

/**
 * {@code (value = array[index + offset]) || (array[index + offset] = support)}
 *
 * @author jimmy
 */
public class PropElementArraySupport extends Propagator<IntVar> {

    private final IntVar value;
    private final IntVar[] array;
    private final IntVar index;
    private final int offset;
    private final int support;

    public PropElementArraySupport(IntVar value, IntVar[] array, IntVar index, int offset, int support) {
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

        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            if (!array[i + offset].contains(support) && !PropUtil.isDomIntersectDom(value, array[i + offset])) {
                index.removeValue(i, this);
            }
        }

        if (index.isInstantiated()) {
            IntVar element = array[index.getValue() + offset];
            if (!element.contains(support)) {
                PropUtil.domSubsetDom(value, element, this);
            }
            ub = element.getUB();
            for (int i = element.getLB(); i <= ub; i = element.nextValue(i)) {
                if (i != support && !value.contains(i)) {
                    element.removeValue(i, this);
                }
            }
            if (value.isInstantiated()) {
                setPassive();
            }
        } else {
            ub = index.getUB();
            for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
                if (array[i + offset].contains(support)) {
                    return;
                }
            }
            ub = value.getUB();
            for (int i = value.getLB(); i <= ub; i = value.nextValue(i)) {
                if (!supportForValue(i)) {
                    value.removeValue(i, this);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            int j = i + offset;
            if (j >= 0 && j < array.length && (array[j].contains(support) || PropUtil.isDomIntersectDom(value, array[j]))) {
                return index.isInstantiated() && value.isInstantiated() && array[j].isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
            }
        }
        return ESat.FALSE;
    }

    @Override
    public String toString() {
        return "elementArraySupport (" + value + ", " + support + ") = " + Arrays.toString(array) + "[" + index + " + " + offset + "]";
    }
}
