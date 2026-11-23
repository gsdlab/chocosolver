package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;
import org.clafer.common.Util;

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
    public int getPropagationConditions(int vIdx) {
        if (isLengthVar(vIdx)) {
            return IntEventType.boundAndInst();
        }
        assert isCharVar(vIdx);
        return IntEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        length.updateLowerBound(0, this);
        length.updateUpperBound(chars.length, this);
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
            if (IntEventType.isInclow(mask) || IntEventType.isInstantiate(mask)) {
                onLengthLB();
            }
            if (IntEventType.isDecupp(mask) || IntEventType.isInstantiate(mask)) {
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
            chars[i].removeValue(terminator, this);
        }
    }

    private void onLengthUB() throws ContradictionException {
        for (int i = length.getUB(); i < chars.length; i++) {
            chars[i].instantiateTo(terminator, this);
        }
    }

    private boolean onCharRemove(int i) throws ContradictionException {
        return !chars[i].contains(terminator)
                && length.updateLowerBound(i + 1, this);
    }

    private boolean onCharInstantiate(int i) throws ContradictionException {
        return chars[i].isInstantiatedTo(terminator)
                && length.updateUpperBound(i, this);
    }

    @Override
    public ESat isEntailed() {
        if (length.getLB() > chars.length || length.getUB() < 0) {
            return ESat.FALSE;
        }
        int leftMostTerminator = chars.length;
        // A possible terminator before leftMostTerminator.
        boolean earlierTerminator = false;
        // Characters after leftMostTerminator are all termiators.
        boolean suffixTerminators = true;
        int rightMostCharacterTerminator = -1;
        for (int i = 0; i < chars.length; i++) {
            if (chars[i].contains(terminator)) {
                if (chars[i].isInstantiatedTo(terminator)) {
                    if (leftMostTerminator == chars.length) {
                        leftMostTerminator = i;
                    }
                    if (i < length.getLB()) {
                        return ESat.FALSE;
                    }
                }
                earlierTerminator |= leftMostTerminator == chars.length;
            } else {
                rightMostCharacterTerminator = i;
                if (i >= length.getUB()) {
                    return ESat.FALSE;
                }
            }
            suffixTerminators &= leftMostTerminator == chars.length || chars[i].isInstantiatedTo(terminator);
        }
        if (length.previousValue(leftMostTerminator + 1) < 0) {
            return ESat.FALSE;
        }
        if (length.nextValue(rightMostCharacterTerminator) > leftMostTerminator) {
            return ESat.FALSE;
        }
        if (!earlierTerminator && suffixTerminators && length.isInstantiatedTo(leftMostTerminator)) {
            return ESat.TRUE;
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "length(" + Arrays.toString(chars) + ") = " + length;
    }
}
