package org.clafer.choco.constraint.propagator;

import org.clafer.collection.Triple;
import org.clafer.common.Util;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

/**
 * Sums a set and |set| &le n. This implementation <b>assumes</b> that the
 * envelope and kernel are sorted, undefined behaviour otherwise.
 *
 * @author jimmy
 */
public class PropSetSum extends Propagator<Variable> {

    private final SetVar set;
    private final IntVar sum;
    private final IntVar card;

    public PropSetSum(SetVar set, IntVar sum, IntVar card) {
        super(new Variable[]{set, sum, card}, PropagatorPriority.LINEAR, false);
        this.set = set;
        this.sum = sum;
        this.card = card;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isSumVar(int idx) {
        return idx == 1;
    }

    private boolean isCardVar(int idx) {
        return idx == 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
        }
        assert isSumVar(vIdx) || isCardVar(vIdx);
        return EventType.INSTANTIATE.mask + EventType.BOUND.mask;
    }

    private Triple<int[], int[], int[]> findKerSmallestLargest(int n) {
        final int[] ker = PropUtil.iterateKer(set);
        final int[] smallest = new int[n];
        final int[] largest = new int[n];

        int chooseSize = set.getEnvelopeSize() - ker.length;
        int highStart = chooseSize - n;

        int index = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (!Util.in(i, ker)) {
                if (index < smallest.length) {
                    smallest[index] = i;
                }
                index++;
                if (index > highStart) {
                    largest[chooseSize - index] = i;
                }
            }
        }
        return new Triple<int[], int[], int[]>(ker, smallest, largest);
    }

    private int lowestMinus(int val, int[] ker, int[] free) {
        int lb = card.getLB() - ker.length;
        int ub = card.getUB() - ker.length;
        int index = 0;
        int low = Util.sum(ker);
        for (int i = 0; i < free.length; i++) {
            if (free[i] != val) {
                if (index < lb) {
                    low += free[i];
                } else if (index < ub) {
                    low += Math.min(free[i], 0);
                } else {
                    break;
                }
                index++;
            }
        }
        return low;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        boolean changed;
        do {
            changed = false;
            final int envSize = set.getEnvelopeSize();
            final int kerSize = set.getKernelSize();

            card.updateLowerBound(kerSize, aCause);
            card.updateUpperBound(envSize, aCause);

            int lbEnd = card.getLB() - kerSize;
            int ubEnd = card.getUB() - kerSize;

            final Triple<int[], int[], int[]> triple = findKerSmallestLargest(ubEnd);
            int[] ker = triple.getFst();
            int[] smallest = triple.getSnd();
            int[] largest = triple.getThd();

            int kerSum = Util.sum(ker);
            int low = kerSum;
            int lowEnd;
            for (lowEnd = 0; lowEnd < ubEnd; lowEnd++) {
                int val = smallest[lowEnd];
                if (!(lowEnd < lbEnd || val < 0)) {
                    break;
                }
                low += val;
            }
            int lowCandidate = // Remove this value when adding anothe.
                    lowEnd > 0 && lowEnd == ubEnd
                    ? smallest[lowEnd - 1]
                    : (lowEnd < smallest.length && smallest[lowEnd] < 0 ? smallest[lowEnd] : 0);

            int high = kerSum;
            int highStart;
            for (highStart = 0; highStart < ubEnd; highStart++) {
                int val = largest[highStart];
                if (!(highStart < lbEnd || val > 0)) {
                    break;
                }
                high += val;
            }
            int highCandidate = // Remove this value when adding another.
                    highStart > 0 && highStart == ubEnd
                    ? largest[highStart - 1]
                    : (highStart < largest.length && largest[highStart] < 0 ? largest[highStart] : 0);
            highStart = envSize - kerSize - highStart;

            sum.updateLowerBound(low, aCause);
            sum.updateUpperBound(high, aCause);

            int lb = sum.getLB();
            int ub = sum.getUB();

            int index = 0;
            for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
                if (!Util.in(i, ker)) {
                    if (index >= lowEnd) {
                        int val = low + i - lowCandidate;
                        // Adding i will never be under the upper bound.
                        if (val > ub) {
                            changed |= set.removeFromEnvelope(i, aCause);
                        }
                    }
                    if (index < highStart) {
                        int val = high + i - highCandidate;
                        // Adding i will never be over the lower bound.
                        if (val < lb) {
                            changed |= set.removeFromEnvelope(i, aCause);
                        }
                    }
                    index++;
                }
            }
        } while (changed);
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        int envSize = set.getEnvelopeSize();
        int kerSize = set.getKernelSize();
        if (kerSize > card.getUB()) {
            return ESat.FALSE;
        }
        int[] ker = PropUtil.iterateKer(set);
        int low = 0;
        int high = 0;
        // The number of elements seen in env but not in ker.
        int index = 0;
        int lowEnd = Math.min(card.getLB(), envSize) - kerSize;
        int highStart = envSize - kerSize - lowEnd;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (Util.in(i, ker)) {
                low += i;
                high += i;
            } else {
                if (index < lowEnd && i < 0) {
                    low += i;
                }
                if (index >= highStart && i > 0) {
                    high += i;
                }
                index++;
            }
        }
        if (sum.getLB() > low || sum.getUB() < high) {
            return ESat.FALSE;
        }
        return low == high && sum.instantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "sum(" + set + ") = " + sum + " where " + card;
    }
}
