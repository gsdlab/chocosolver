package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.clafer.common.Util;
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
public class PropLength extends Propagator<IntVar> {

    private final IntVar[] chars;
    private final IntVar length;
    private final int terminator;

    public PropLength(IntVar[] chars, IntVar length) {
        this(chars, length, 0);
    }

    public PropLength(IntVar[] chars, IntVar length, int terminator) {
        super(Util.cons(length, chars), PropagatorPriority.LINEAR, true);
        this.chars = chars;
        this.length = length;
        this.terminator = terminator;
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
            return IntEventType.boundAndInst();
        }
        assert isCharVar(vIdx);
        return IntEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        length.updateLowerBound(0, aCause);
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
            if (IntEventType.isInclow(mask)) {
                onLengthLB();
            }
            if (IntEventType.isDecupp(mask)) {
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
            chars[i].removeValue(terminator, aCause);
        }
    }

    private void onLengthUB() throws ContradictionException {
        for (int i = length.getUB(); i < chars.length; i++) {
            chars[i].instantiateTo(terminator, aCause);
        }
    }

    private boolean onCharRemove(int i) throws ContradictionException {
        return !chars[i].contains(terminator)
                && length.updateLowerBound(i + 1, aCause);
    }

    private boolean onCharInstantiate(int i) throws ContradictionException {
        return chars[i].isInstantiatedTo(terminator)
                && length.updateUpperBound(i, aCause);
    }

    @Override
    public ESat isEntailed() {
        if (length.getLB() > chars.length || length.getUB() < 0) {
            return ESat.FALSE;
        }
        for (int i = 0; i < length.getLB(); i++) {
            if (chars[i].isInstantiatedTo(terminator)) {
                return ESat.FALSE;
            }
        }
        for (int i = length.getUB(); i < chars.length; i++) {
            if (!chars[i].contains(terminator)) {
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
