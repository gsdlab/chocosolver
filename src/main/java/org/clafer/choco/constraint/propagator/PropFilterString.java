package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 * result = [string !! i | i <- set].
 *
 * Pads result with extra negative one if necessary.
 *
 * @author jimmy
 */
public class PropFilterString extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar setCard;
    private final int offset;
    // Sorted in decreasing order. Non-negatives.
    private final IntVar[] string;
    private final IntVar[] result;

    public PropFilterString(SetVar set, IntVar setCard, int offset, IntVar[] string, IntVar[] result) {
        super(buildArray(set, setCard, string, result), PropagatorPriority.QUADRATIC, false);
        this.set = set;
        this.setCard = setCard;
        this.offset = offset;
        this.string = string;
        this.result = result;
    }

    public static Variable[] buildArray(SetVar set, IntVar setCard, IntVar[] string, IntVar[] result) {
        Variable[] array = new Variable[2 + string.length + result.length];
        array[0] = set;
        array[1] = setCard;
        System.arraycopy(string, 0, array, 2, string.length);
        System.arraycopy(result, 0, array, 2 + string.length, result.length);
        return array;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isSetCardVar(int idx) {
        return idx == 1;
    }

    private boolean isStringVar(int idx) {
        return idx >= 1 && idx < string.length + 2;
    }

    private int getStringVarIndex(int idx) {
        assert isStringVar(idx);
        return idx - 2;
    }

    private boolean isResultVar(int idx) {
        return idx >= string.length + 2;
    }

    private int getResultVarIndex(int idx) {
        assert isResultVar(idx);
        return idx - 2 - string.length;
    }

    @Override
    public boolean advise(int idxVarInProp, int mask) {
        if (isStringVar(idxVarInProp)) {
            return set.envelopeContains(getStringVarIndex(idxVarInProp) + offset);
        }
        return super.advise(idxVarInProp, mask);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        if (isSetCardVar(vIdx)) {
            return IntEventType.boundAndInst();
        }
        return IntEventType.all();
    }

    private boolean subset(IntVar sub, IntVar[] sups, int from, int to) throws ContradictionException {
        boolean changed = false;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            boolean found = false;
            for (int i = from; i < to; i++) {
                if (sups[i].contains(val)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                changed |= sub.removeValue(val, aCause);
            }
        }
        return changed;
    }

    private boolean subset(IntVar sub, IntVar[] sups, int[] indices, int low, int to) throws ContradictionException {
        boolean changed = false;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            boolean found = false;
            for (int i = low; i < to; i++) {
                if (sups[indices[i]].contains(val)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                changed |= sub.removeValue(val, aCause);
            }
        }
        return changed;
    }

    private boolean subsetOrNegativeOne(IntVar sub, IntVar[] sups, int[] indices, int from, int to) throws ContradictionException {
        boolean changed = false;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (val != -1) {
                boolean found = false;
                for (int i = from; i < to; i++) {
                    assert i < indices.length;
                    if (sups[indices[i]].contains(val)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    changed |= sub.removeValue(val, aCause);
                }
            }
        }
        return changed;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (set.getKernelSize() > result.length) {
            contradiction(set, "Too many in kernel");
        }

        int[] env = new int[set.getEnvelopeSize()];
        int[] kerIndices = new int[set.getKernelSize()];

        boolean changed;
        do {
            changed = false;
            // The number of ker elements seen.
            int kerIndex = 0;
            // The number of env elements seen.
            int envIndex = 0;
            for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
                int x = i - offset;
                if (x < 0 || x >= string.length) {
                    set.removeFromEnvelope(i, aCause);
                } else {
                    env[envIndex] = x;
                    if (set.kernelContains(i)) {
                        kerIndices[kerIndex] = envIndex;
                        changed |= subset(string[x], result, kerIndex, Math.min(envIndex + 1, result.length));
                        envIndex++;
                        kerIndex++;
                    } else {
                        boolean found = false;
                        for (int j = kerIndex; j < result.length && j <= envIndex; j++) {
                            if (PropUtil.isDomIntersectDom(string[x], result[j])) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            changed |= set.removeFromEnvelope(i, aCause);
                        } else {
                            envIndex++;
                        }
                    }
                }
            }
            assert envIndex <= env.length;
            assert kerIndex == kerIndices.length;

            int lb = setCard.getLB();
            int ub = setCard.getUB();

            for (; lb < result.length && !result[lb].contains(-1); lb++) {
            }
            changed |= setCard.updateLowerBound(lb, aCause);
            for (int i = 0; i < result.length; i++) {
                if (i < kerIndices.length) {
                    changed |= subset(result[i], string, env, i, kerIndices[i] + 1);
                } else if (i < lb) {
                    changed |= subset(result[i], string, env, i, envIndex);
                } else if (i > ub) {
                    changed |= result[i].instantiateTo(-1, aCause);
                } else {
                    changed |= subsetOrNegativeOne(result[i], string, env, i, envIndex);
                }
            }
        } while (changed);
    }

    @Override
    public ESat isEntailed() {
        int index = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END && set.kernelContains(i); i = set.getEnvelopeNext(), index++) {
            if (index >= result.length) {
                return ESat.FALSE;
            }
            int x = i - offset;
            if (x < 0 || x >= string.length) {
                return ESat.FALSE;
            }
            if (string[x].isInstantiated() && result[index].isInstantiated()
                    && string[x].getValue() != result[index].getValue()) {
                return ESat.FALSE;
            }
        }
        for (int i = set.getEnvelopeSize(); i < result.length; i++) {
            if (!result[i].contains(-1)) {
                return ESat.FALSE;
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "filter(" + set + " >> " + offset + ", " + Arrays.toString(string) + ", " + Arrays.toString(result) + ")";
    }
}
