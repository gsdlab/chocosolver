package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.memory.IEnvironment;
import org.clafer.common.Util;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISet;
import org.chocosolver.util.objects.setDataStructures.SetFactory;
import org.chocosolver.util.objects.setDataStructures.SetType;

/**
 *
 * @author jimmy
 */
public class PropCountNotEqual extends Propagator<IntVar> {

    private final int value;
    private final IntVar[] array;
    private final IntVar count;
    private final ISet possibles, mandatories;

    public PropCountNotEqual(int value, IntVar[] array, IntVar count) {
        super(Util.snoc(array, count), PropagatorPriority.LINEAR, true);
        this.value = value;
        this.array = array;
        this.count = count;
        this.possibles = SetFactory.makeStoredSet(SetType.BITSET, array.length, solver);
        this.mandatories = SetFactory.makeStoredSet(SetType.BITSET, array.length, solver);
    }

    boolean isArrayVar(int idx) {
        return idx < array.length;
    }

    int getArrayVarIndex(int idx) {
        return idx;
    }

    boolean isCountVar(int idx) {
        return idx == array.length;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isCountVar(vIdx)) {
            return IntEventType.boundAndInst();
        }
        assert isArrayVar(vIdx);
        return IntEventType.all();
    }

    private void filter() throws ContradictionException {
        count.updateLowerBound(mandatories.getSize(), aCause);
        count.updateUpperBound(mandatories.getSize() + possibles.getSize(), aCause);
        if (count.isInstantiated()) {
            int nb = count.getValue();
            if (possibles.getSize() + mandatories.getSize() == nb) {
                for (int j = possibles.getFirstElement(); j >= 0; j = possibles.getNextElement()) {
                    // vars[j] might be a bounded variabled
                    if (vars[j].removeValue(value, aCause)) {
                        possibles.remove(j);
                    }
                }
                if (possibles.isEmpty()) {
                    setPassive();
                }
            } else if (mandatories.getSize() == nb) {
                for (int j = possibles.getFirstElement(); j >= 0; j = possibles.getNextElement()) {
                    vars[j].instantiateTo(value, aCause);
                }
                setPassive();
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        mandatories.clear();
        possibles.clear();
        for (int i = 0; i < array.length; i++) {
            IntVar v = array[i];
            if (!v.contains(value)) {
                mandatories.add(i);
            } else if (!v.isInstantiatedTo(value)) {
                possibles.add(i);
            }
        }
        filter();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isArrayVar(idxVarInProp)) {
            IntVar var = vars[idxVarInProp];
            if (possibles.contain(idxVarInProp)) {
                if (!var.contains(value)) {
                    possibles.remove(idxVarInProp);
                    mandatories.add(idxVarInProp);
                    filter();
                } else if (var.isInstantiated()) {
                    possibles.remove(idxVarInProp);
                    filter();
                }
            }
        } else {
            filter();
        }
    }

    @Override
    public ESat isEntailed() {
        int min = 0;
        int max = 0;
        for (IntVar v : array) {
            if (!v.contains(value)) {
                min++;
                max++;
            } else if (!v.isInstantiated()) {
                max++;
            }
        }
        if (count.getUB() < min || count.getLB() > max) {
            return ESat.FALSE;
        }
        return count.isInstantiated() && min == max ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "count!=" + value + "(" + Arrays.toString(array) + ", " + count + ")" + "::: " + mandatories + ":::" + possibles;
    }
}
