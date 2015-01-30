package org.clafer.choco.constraint.propagator;

import org.clafer.collection.Triple;
import org.clafer.common.Util;
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
 * Sums a set and |set| &le n. This implementation <b>assumes</b> that the
 * envelope and kernel are sorted, undefined behaviour otherwise.
 *
 * @author jimmy
 */
public class PropSetSum extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar setCard;
    private final IntVar sum;

    public PropSetSum(SetVar set, IntVar setCard, IntVar sum) {
        super(new Variable[]{set, setCard, sum}, PropagatorPriority.LINEAR, false);
        this.set = set;
        this.setCard = setCard;
        this.sum = sum;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isSetCardVar(int idx) {
        return idx == 1;
    }

    private boolean isSumVar(int idx) {
        return idx == 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        assert isSumVar(vIdx) || isSetCardVar(vIdx);
        return IntEventType.boundAndInst();
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
        return new Triple<>(ker, smallest, largest);
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        boolean changed;
        do {
            changed = false;
            final int envSize = set.getEnvelopeSize();
            final int kerSize = set.getKernelSize();

            setCard.updateLowerBound(kerSize, aCause);
            setCard.updateUpperBound(envSize, aCause);

            int lbEnd = setCard.getLB() - kerSize;
            int ubEnd = setCard.getUB() - kerSize;

            final Triple<int[], int[], int[]> triple = findKerSmallestLargest(ubEnd);
            int[] ker = triple.getFst();
            int[] smallest = triple.getSnd();
            int[] largest = triple.getThd();

            int kerSum = Util.sum(ker);
            int low = kerSum;
            int lowEnd;
            int lowCandidate = 0; // Remove this value when adding another.
            for (lowEnd = 0; lowEnd < ubEnd; lowEnd++) {
                int val = smallest[lowEnd];
                if (!(lowEnd < lbEnd || val < 0)) {
                    // Still room left. Don't remove lowCandidate if it's negative.
                    lowCandidate = Math.max(lowCandidate, 0);
                    break;
                }
                lowCandidate = val;
                low += val;
            }
            int high = kerSum;
            int highStart;
            int highCandidate = 0; // Remove this value when adding another.
            for (highStart = 0; highStart < ubEnd; highStart++) {
                int val = largest[highStart];
                if (!(highStart < lbEnd || val > 0)) {
                    // Still room left. Don't remove highCandidate if it's positive.
                    highCandidate = Math.min(highCandidate, 0);
                    break;
                }
                highCandidate = val;
                high += val;
            }
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
    public ESat isEntailed() {
        int envSize = set.getEnvelopeSize();
        int kerSize = set.getKernelSize();
        if (kerSize > setCard.getUB()) {
            return ESat.FALSE;
        }
        int[] ker = PropUtil.iterateKer(set);
        int low = 0;
        int high = 0;
        // The number of elements seen in env but not in ker.
        int index = 0;
        int lowEnd = Math.max(setCard.getLB(), envSize) - kerSize;
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
        if (sum.getLB() > high || sum.getUB() < low) {
            return ESat.FALSE;
        }
        return low == high && sum.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "sum(" + set + ") = " + sum + " where " + setCard;
    }
}
