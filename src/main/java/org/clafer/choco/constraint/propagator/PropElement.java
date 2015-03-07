package org.clafer.choco.constraint.propagator;

import gnu.trove.map.hash.TIntIntHashMap;
import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.delta.IIntDeltaMonitor;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropElement extends Propagator<IntVar> {

    private final IntVar value;
    private final IntVar[] array;
    private final IIntDeltaMonitor[] arrayD;
    private final IntVar index;
    private final int offset;
    // Maps from an index in the array to an integer in value.
    private final TIntIntHashMap arraySupports;
    // Maps from an integer in value to an index in array.
    private final TIntIntHashMap valueSupports;

    public PropElement(IntVar value, IntVar[] array, IntVar index, int offset) {
        super(buildArray(value, array, index), PropagatorPriority.LINEAR, true);
        this.value = value;
        this.array = array;
        this.arrayD = PropUtil.monitorDeltas(array, aCause);
        this.index = index;
        this.offset = offset;

        this.arraySupports = new TIntIntHashMap(array.length);
        this.valueSupports = new TIntIntHashMap(value.getDomainSize());
    }

    private static IntVar[] buildArray(IntVar value, IntVar[] array, IntVar index) {
        IntVar[] variables = new IntVar[array.length + 2];
        variables[0] = value;
        variables[1] = index;
        System.arraycopy(array, 0, variables, 2, array.length);
        return variables;
    }

    private boolean isValueVar(int idx) {
        return idx == 0;
    }

    private boolean isIndexVar(int idx) {
        return idx == 1;
    }

    private boolean isArrayVar(int idx) {
        return idx >= 2;
    }

    private int getArrayVarIndex(int idx) {
        return idx - 2;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return IntEventType.all();
    }

    private boolean supportForArray(int index) {
        int support = arraySupports.get(index);
        if (value.contains(support) && array[index].contains(support)) {
            return true;
        }
        int i = PropUtil.getDomIntersectDom(value, array[index]);
        if (i == Integer.MAX_VALUE) {
            return false;
        }
        arraySupports.put(index, i);
        return true;
    }

    private boolean supportForValue(int value) {
        int support = valueSupports.get(value);
        if (index.contains(support - offset) && array[support].contains(value)) {
            return true;
        }
        int ub = index.getUB();
        for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
            if (array[i + offset].contains(value)) {
                valueSupports.put(value, i + offset);
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
            if (!supportForArray(i + offset)) {
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
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isValueVar(idxVarInProp)) {
            int ub = index.getUB();
            for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
                if (!supportForArray(i + offset)) {
                    index.removeValue(i, aCause);
                }
            }
        } else if (isIndexVar(idxVarInProp)) {
            int ub = value.getUB();
            for (int i = value.getLB(); i <= ub; i = value.nextValue(i)) {
                if (!supportForValue(i)) {
                    value.removeValue(i, aCause);
                }
            }
        } else {
            assert isArrayVar(idxVarInProp);
            int i = getArrayVarIndex(idxVarInProp);
            if (index.contains(i - offset)) {
                if (!supportForArray(i)) {
                    index.removeValue(i - offset, aCause);
                }
                arrayD[i].freeze();
                arrayD[i].forEachRemVal(new IntProcedure() {
                    @Override
                    public void execute(int rem) throws ContradictionException {
                        if (!supportForValue(rem)) {
                            value.removeValue(rem, aCause);
                        }
                    }
                });
                arrayD[i].unfreeze();
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
        return "Element {" + Arrays.toString(array) + "}[" + index + " + " + offset + "] = " + value;
    }
}
