package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSamePrefix extends Propagator<IntVar> {

    private final IntVar length;
    private final IntVar[] string1;
    private final IntVar[] string2;

    public PropSamePrefix(IntVar length, IntVar[] string1, IntVar[] string2) {
        super(buildArray(length, string1, string2), PropagatorPriority.LINEAR, true);
        this.length = length;
        this.string1 = string1;
        this.string2 = string2;
    }

    private static IntVar[] buildArray(IntVar length, IntVar[] string1, IntVar[] string2) {
        IntVar[] array = new IntVar[1 + string1.length + string2.length];
        array[0] = length;
        System.arraycopy(string1, 0, array, 1, string1.length);
        System.arraycopy(string2, 0, array, 1 + string1.length, string2.length);
        return array;
    }

    private boolean isLengthVar(int idx) {
        return idx == 0;
    }

    private boolean isString1Var(int idx) {
        return idx > 0 && idx <= string1.length;
    }

    private int getString1VarIndex(int idx) {
        return idx - 1;
    }

    private boolean isString2Var(int idx) {
        return idx > string1.length;
    }

    private int getString2VarIndex(int idx) {
        return idx - string1.length - 1;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        if (isLengthVar(vIdx)) {
            return IntEventType.boundAndInst();
        }
        assert isString1Var(vIdx) || isString2Var(vIdx);
        return IntEventType.all();
    }

    private boolean neq(int i) {
        return (string1[i].isInstantiated() && !string2[i].contains(string1[i].getValue()))
                || (string2[i].isInstantiated() && !string1[i].contains(string2[i].getValue()));
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        length.updateUpperBound(Math.min(string1.length, string2.length), aCause);
        for (int i = 0; i < length.getLB(); i++) {
            PropUtil.domSubsetDom(string1[i], string2[i], aCause);
            PropUtil.domSubsetDom(string2[i], string1[i], aCause);
        }
        for (int i = length.getLB(); i < length.getUB(); i++) {
            if (neq(i)) {
                length.updateUpperBound(i, aCause);
                break;
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isLengthVar(idxVarInProp)) {
            for (int i = 0; i < length.getLB(); i++) {
                PropUtil.domSubsetDom(string1[i], string2[i], aCause);
                PropUtil.domSubsetDom(string2[i], string1[i], aCause);
            }
        } else if (isString1Var(idxVarInProp)) {
            int id = getString1VarIndex(idxVarInProp);
            if (id < length.getLB()) {
                PropUtil.domSubsetDom(string2[id], string1[id], aCause);
            }
            if (id < length.getUB() && neq(id)) {
                length.updateUpperBound(id, aCause);
            }
        } else {
            assert isString2Var(idxVarInProp);
            int id = getString2VarIndex(idxVarInProp);
            if (id < length.getLB()) {
                PropUtil.domSubsetDom(string1[id], string2[id], aCause);
            }
            if (id < length.getUB() && neq(id)) {
                length.updateUpperBound(id, aCause);
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (length.getLB() > string1.length || length.getLB() > string2.length) {
            return ESat.FALSE;
        }
        boolean isCompletelyInstantiated = true;
        for (int i = 0; i < length.getLB(); i++) {
            if (neq(i)) {
                return ESat.FALSE;
            }
            isCompletelyInstantiated = isCompletelyInstantiated
                    && string1[i].isInstantiated() && string2[i].isInstantiated();
        }
        isCompletelyInstantiated = isCompletelyInstantiated && length.isInstantiated();
        return isCompletelyInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "samePrefix(" + length + ", " + Arrays.toString(string1) + ", " + Arrays.toString(string2) + ")";
    }
}
