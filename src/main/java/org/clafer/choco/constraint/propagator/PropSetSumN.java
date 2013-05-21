package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

/**
 * Sums a set. |set| &le n.
 *
 * Assumption: the env and ker are sorted.
 *
 * @author jimmy
 */
public class PropSetSumN extends Propagator<Variable> {

    private final SetVar set;
    private final IntVar sum;
    private final int n;

    public PropSetSumN(SetVar set, IntVar sum, int n) {
        super(new Variable[]{set, sum}, PropagatorPriority.LINEAR);
        if (n <= 0) {
            throw new IllegalArgumentException();
        }
        this.set = set;
        this.sum = sum;
        this.n = n;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isSumVar(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
        }
        assert isSumVar(vIdx);
        return EventType.INSTANTIATE.mask + EventType.BOUND.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        propagate(0, 0);
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        int envSize = set.getEnvelopeSize();
        int kerSize = set.getKernelSize();
        if (kerSize > n) {
            contradiction(set, "|" + set + "| > " + n);
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
        int lowEnd = Math.min(n, envSize) - kerSize;
        // The [highStart-choosSize) integers in env-ker are the highest integers.
        int highStart = chooseSize - lowEnd;
        // The n + 1 highest integer, or 0 if not positive.
        int highCandidate = 0;
        // The highest n positive integers in order.
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
System.out.println("low: " + low + ", high: "+  high);
        sum.updateLowerBound(low, aCause);
        sum.updateUpperBound(high, aCause);

        int lb = sum.getLB();
        int ub = sum.getUB();

        boolean again = false;
        index = 0;
        // sum of the lowest n - 1 negative integers that can be chosen.
        int lowNMinusOne = kerSum;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (!Util.in(i, ker)) {
                if (index < lowEnd - 1) {
                    lowNMinusOne += Math.min(0, i);
                } else if ( // Is it possible to add i to the kernel?
                        lowNMinusOne + i > ub) {
                    // With i, it is impossible to stay below the upper bound.
                    // Remove it from the envelope.
                    System.out.println("remove: " + i);
                    again |= set.removeFromEnvelope(i, aCause);
                }
                index++;
            }
        }
        for (int i = highChoose.length - 1; i >= 0
                // Is it possible to leave i out of the kernel?
                // Replace i with the candidate.
                && high - highChoose[i] + highCandidate < lb;
                i--) {
            // Without i, it is impossible to reach the lower bound.
            // Add it to the kernel.
            again |= set.addToKernel(highChoose[i], aCause);
        }
        if (again) {
            // Changed some values, need to repropagate.
            propagate(0, 0);
        }
    }

    @Override
    public ESat isEntailed() {
        int envSize = set.getEnvelopeSize();
        int kerSize = set.getKernelSize();
        if (kerSize > n) {
            return ESat.FALSE;
        }
        int[] ker = PropUtil.iterateKer(set);
        int low = 0;
        int high = 0;
        // The number of elements seen in env but not in ker.
        int index = 0;
        int lowEnd = Math.min(n, envSize - kerSize);
        int highStart = envSize - kerSize - lowEnd;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (Util.in(i, ker)) {
                low += i;
                high += i;
            } else {
                if (index < lowEnd && i < 0) {
                    low -= i;
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
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    private static int sumKer(SetVar set) {
        int sum = 0;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            sum += i;
        }
        return sum;
    }

    private static LowHigh lowHigh(SetVar set, int k) {
        int envSize = set.getEnvelopeSize();
        int kerSize = set.getKernelSize();

        assert kerSize < k;
        if (kerSize == Math.min(k, envSize)) {
            int kerSum = sumKer(set);
            int[] choose = new int[0];
            return new LowHigh(kerSum, choose, choose);
        }

        int[] ker = PropUtil.iterateKer(set);
        // The sum of the lowest, negative integers that can be chosen.
        int low = 0;
        // The sum of the highest, postiive integers that can be chosen.
        int high = 0;
        // The number of elements seen in env but not in ker.
        int index = 0;
        // The next lowest integer if negative, or 0.
        int lowCandidate = 0;
        // The next highest integer if positive, or 0.
        int highCandidate = 0;
        int chooseSize = envSize - kerSize;
        int lowEnd = Math.min(k, chooseSize);
        int highStart = chooseSize - lowEnd;
        int[] lowChoose = new int[lowEnd];
        int[] highChoose = new int[lowChoose.length];
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (!Util.in(i, ker)) {
                if (index < lowEnd && i < 0) {
                    // Found a min.
                    lowChoose[index] = i;
                }
                index++;
                if (index > highStart && i > 0) {
                    highChoose[chooseSize - index] = i;
                }
            }
        }
        return new LowHigh(Util.sum(ker), lowChoose, highChoose);
    }

    @Override
    public String toString() {
        return "sum(" + set + ") = " + sum + " where |" + set.getName() + "| <= " + n;
    }

    private static class LowHigh {

        /**
         * The sum of the integers already chosen.
         */
        private final int sumKer;
        /**
         * The set of negative integers that still can be chosen to make the sum
         * as small as possible for |set| &le <b>k</b>. The elements are in
         * ascending order. The end of the array may or may not be padded extra
         * zeroes.
         */
        private final int[] lowChoose;
        /**
         * The set of positive integers that still can be chosen to make the sum
         * as large as possible for |set| &le <b>k</b>. The elements are in
         * ascending order. The end of the array may or may not be padded extra
         * zeroes.
         */
        private final int[] highChoose;

        LowHigh(int sumKer, int[] lowChoose, int[] highChoose) {
            this.sumKer = sumKer;
            this.lowChoose = lowChoose;
            this.highChoose = highChoose;
        }
    }
}
