package org.clafer.choco.constraint.propagator;

import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

/**
 * sum = Sum{i in 1..set.size} weight[set[i]] * scale ^ (set.size - i).
 *
 * @author jimmy
 */
public class PropSumWeight extends Propagator<Variable> {

    // Only non-negative integers.
    // SET MUST BE CONTINUOUS
    // For example, if 3 and 6 are in the kernel, then 4 and 5 must be in the
    // kernel as well.
    private final SetVar set;
    // private final int lowCard;
    // private final int highCard;
    // Only positive integers.
    private final IntVar[] weight;
    // Must be non-negative.
    // SCALE MUST BE LARGER THAN ANY ONE WEIGHT
    private final int scale;
    // Must be non-negative.
    private final IntVar sum;

    public PropSumWeight(SetVar set, IntVar[] weight, int scale, IntVar sum) {
        super(buildArray(set, weight, sum), PropagatorPriority.LINEAR, true);
        this.set = set;
        this.weight = weight;
        this.scale = scale;
        this.sum = sum;
    }

    public static Variable[] buildArray(SetVar set, IntVar[] weight, IntVar sum) {
        Variable[] array = new Variable[weight.length + 2];
        array[0] = set;
        array[1] = sum;
        System.arraycopy(weight, 0, array, 2, weight.length);
        return array;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isSumVar(int idx) {
        return idx == 1;
    }

    private boolean isWeightVar(int idx) {
        return idx >= 2;
    }

    private int getWeightVarIndex(int idx) {
        assert isWeightVar(idx);
        return idx - 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
        }
        return EventType.INT_ALL_MASK();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int minSum = 0;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            minSum = minSum * scale + weight[i].getLB();
        }
        int maxSum = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            maxSum = maxSum * scale + weight[i].getUB();
        }
        sum.updateLowerBound(minSum, aCause);
        sum.updateUpperBound(maxSum, aCause);
        int lb = sum.getLB();
        int ub = sum.getUB();
//        boolean changed = false;
//        int minSumSeen = minSum;
//        int maxSumSeen = maxSum;
//        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
//            maxSumSeen = maxSumSeen * scale + weight[i].getUB();
//            if (!set.kernelContains(i)) {
//                int maxSumRest = maxSum / maxSumSeen;
//                // Not adding weight[i] will never be over the lower bound
//                if (((maxSumSeen - weight[i].getLB()) / scale) * maxSumRest < lb) {
//                    if (set.addToKernel(i, aCause)) {
//                        changed = true;
//                    }
//                }
//                int minSumRest = minSum / minSumSeen;
//                // Adding weight[i] will never be under the upper bound
//                if ((minSumSeen * scale + weight[i].getLB()) * minSumRest > ub) {
//                    if (set.removeFromEnvelope(i, aCause)) {
//                        changed = true;
//                    }
//                }
//            } else {
//                minSumSeen = minSumSeen * scale + weight[i].getLB();
//            }
//        }
//        if (changed) {
//            propagate(0);
//        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        int minSum = 0;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            minSum = minSum * scale + weight[i].getLB();
        }
        if (minSum > sum.getUB()) {
            return ESat.FALSE;
        }
        int maxSum = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            maxSum = maxSum * scale + weight[i].getUB();
        }
        if (maxSum < sum.getLB()) {
            return ESat.FALSE;
        }
        return minSum == maxSum && sum.instantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }
}
