package org.clafer.choco.constraint.propagator;

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

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int envSize = set.getEnvelopeSize();
        int kerSize = set.getKernelSize();
        if (kerSize > card.getUB()) {
            contradiction(set, "|" + set + "| > " + card);
        }

        final int[] ker = PropUtil.iterateKer(set);
        final int kerSum = Util.sum(ker);
        // The sum of the lowest n negative integers that can be chosen.
        int low = kerSum;
        // The sum of the highest n positive integers that can be chosen.
        int high = kerSum;
        // The number of elements seen in env but not in ker.
        int index = 0;
        // The size of the set of integers to still choose from.
        int chooseSize = envSize - kerSize;
        // The [0-lowEnd) integers in env-ker are the lowest integers.
        int lowEnd = Math.min(card.getUB(), envSize) - kerSize;
        // The [highStart-choosSize) integers in env-ker are the highest integers.
        int highStart = chooseSize - lowEnd;
        // The n + 1 highest integer, or 0 if not positive.
        int highCandidate = 0;
        // The highest n positive integers in descending order.
        int[] highChoose = new int[chooseSize - highStart];
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (!Util.in(i, ker)) {
                if (index < lowEnd) {
                    low += Math.min(0, i);
                }
                index++;
                if (index == highStart) {
                    highCandidate = Math.max(0, i);
                } else if (index > highStart && i > 0) {
                    int j = Math.max(0, i);
                    highChoose[chooseSize - index] = j;
                    high += j;
                }
            }
        }

        sum.updateLowerBound(low, aCause);
        sum.updateUpperBound(high, aCause);

        int lb = sum.getLB();
        int ub = sum.getUB();

        boolean changed = false;
        index = 0;
        // sum of the lowest n - 1 negative integers that can be chosen.
        int lowNMinusOne = kerSum;
        // sum of the highest n - 1 positive integers that can be chosen.
        int highNMinusOne = high - (highChoose.length == 0 ? 0 : highChoose[highChoose.length - 1]);
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (!Util.in(i, ker)) {
                if (index < lowEnd - 1) {
                    lowNMinusOne += Math.min(0, i);
                } else if ( // Is it possible to add i to the kernel?
                        // Adding i will never be under the upper bound
                        lowNMinusOne + i > ub
                        // Adding i will never be over the lower bound
                        || highNMinusOne + i < lb) {
                    // With i, it is impossible to stay below the upper bound.
                    // Remove it from the envelope.
                    changed |= set.removeFromEnvelope(i, aCause);
                }
                index++;
            }
        }
        for (int i = highChoose.length - 1; i >= 0
                // Is it possible to leave i out of the kernel?
                // Replace i with the candidate.
                // Not adding highChoose[i] will never be over the lower bound
                && high - highChoose[i] + highCandidate < lb;
                i--) {
            // Without i, it is impossible to reach the lower bound.
            // Add it to the kernel.
            changed |= set.addToKernel(highChoose[i], aCause);
        }
        if (changed) {
            // Changed some values, need to repropagate.
            propagate(evtmask);
        }
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
        int lowEnd = Math.min(card.getUB(), envSize - kerSize);
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
        return "sum(" + set + ") = " + sum + " where |" + set.getName() + "| <= " + card;
    }
}
