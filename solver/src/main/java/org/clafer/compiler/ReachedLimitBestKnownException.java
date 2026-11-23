package org.clafer.compiler;

import org.clafer.instance.InstanceModel;

/**
 * Resource limit reached on an optimization problem with the best known
 * solution thus far.
 *
 * <pre>
 * solver.limitTime(1000);
 * try {
 *   solver.find();
 * } catch (ReachedLimitBestKnownException rle) {
 *   // This is an optimization problem and 1000ms has passed.
 *   // Although the solver did not complete, it returned the best known
 *   // (but possibly non-optimal) instance after 1000ms. This instance is
 *   // stored in the rle exception.
 *   while (rle.find()) {
 *     System.out.println("Approximate solutions: " + rle.instance());
 *   }
 * } catch (ReachedLimitException rle) {
 *   // 1000ms has passed.
 * }
 * </pre>
 *
 * @author jimmy
 */
public class ReachedLimitBestKnownException extends ReachedLimitException implements OptimalInstanceIterator {

    private final InstanceModel[] instances;
    private final int[][] objectiveValues;
    private int count = 0;

    public ReachedLimitBestKnownException(InstanceModel[] instances, int[][] objectiveValues) {
        this.instances = instances;
        this.objectiveValues = objectiveValues;
    }

    public ReachedLimitBestKnownException(InstanceModel instance, int[] objectiveValues) {
        this.instances = new InstanceModel[]{instance};
        this.objectiveValues = new int[][]{objectiveValues};
    }

    @Override
    public boolean find() {
        if (count < instances.length) {
            count++;
            return true;
        }
        return false;
    }

    @Override
    public InstanceModel instance() {
        if (count == 0 || count > instances.length) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return instances[count - 1];
    }

    @Override
    public int[] optimalValues() {
        if (count == 0 || count > instances.length) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return objectiveValues[count - 1];
    }

    @Override
    public int instanceCount() {
        return count;
    }
}
