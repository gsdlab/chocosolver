package org.clafer.constraint.propagator;

import org.clafer.Util;
import org.clafer.collection.IntPair;
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
 * @author jimmy
 */
public class PropSumSetN extends Propagator<Variable> {

    private final SetVar set;
    private final IntVar sum;
    private final int n;

    public PropSumSetN(SetVar set, IntVar sum, int n) {
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
        if (set.getKernelSize() > n) {
            contradiction(set, "|" + set + "| > " + n);
        }
        final IntPair lowHigh = lowHigh(set, n);
        final int low = lowHigh.getFst();
        final int high = lowHigh.getSnd();
        sum.updateLowerBound(low, aCause);
        sum.updateUpperBound(high, aCause);
        boolean again = false;
        int lb = sum.getLB();
        int ub = sum.getUB();
        for (int j = set.getEnvelopeFirst(); j != SetVar.END; j = set.getEnvelopeNext()) {
            if (high - j < lb) {
                if (set.addToKernel(j, aCause)) {
                    again = true;
                }
            } else if (low + j > ub) {
                if (set.removeFromEnvelope(j, aCause)) {
                    again = true;
                }
            }
        }
        if (again) {
            propagate(0, 0);
        }
    }

    @Override
    public ESat isEntailed() {
        if (set.getKernelSize() > n) {
            return ESat.FALSE;
        }
        final IntPair lowHigh = lowHigh(set, n);
        if (sum.getLB() > lowHigh.getSnd() || sum.getUB() < lowHigh.getFst()) {
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

    private static IntPair lowHigh(SetVar set, int n) {
        int kerSize = set.getKernelSize();
        int envSize = set.getEnvelopeSize();
        int top = Math.min(n, envSize);
        assert kerSize <= top;
        if (kerSize == n) {
            int sumKer = sumKer(set);
            return new IntPair(sumKer, sumKer);
        }

        int[] ker = PropUtil.iterateKer(set);
        int sumKer = Util.sum(ker);

        int low = sumKer;
        int[] max = new int[top - kerSize];

        int index = 0;
        int j = set.getEnvelopeFirst();
        for (; index < max.length; j = set.getEnvelopeNext()) {
            assert j != SetVar.END;
            // Since the array is sorted, we can do binary search, however, the array
            // is bounded by min(top, set.getKernelSize()), hence it is typically very
            // small, so sequential search is better.
            if (!Util.in(j, ker)) {
                if (j < 0) {
                    low += j;
                }
                max[index] = j;
                index++;
            }
        }

        index = 0;
        for (; j != SetVar.END; j = set.getEnvelopeNext()) {
            if (!Util.in(j, ker)) {
                max[index++] = j;
                if (index == max.length) {
                    index = 0;
                }
            }
        }

        int high = sumKer;
        // max is a sorted circular array. Traverse backwards to visit in descending
        // order.
        for (int k = index - 1; k >= 0; k--) {
            int val = max[k];
            if (val < 0) {
                return new IntPair(low, high);
            }
            high += val;
        }
        for (int k = max.length - 1; k >= index; k--) {
            int val = max[k];
            if (val < 0) {
                break;
            }
            high += val;
        }
        return new IntPair(low, high);
    }

    @Override
    public String toString() {
        return "sum(" + set + ") = " + set + " where |" + set.getName() + "| <= " + n;
    }
}
