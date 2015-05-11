package org.clafer.compiler;

import org.clafer.instance.InstanceModel;

/**
 * Resource limit reached on an optimization problem with the best known
 * solution thus far.
 *
 * @author jimmy
 */
public class ReachedLimitBestKnownException extends ReachedLimitException {

    private final InstanceModel instance;
    private final int[] objectiveValues;

    public ReachedLimitBestKnownException(InstanceModel instance, int[] objectiveValues) {
        this.instance = instance;
        this.objectiveValues = objectiveValues;
    }

    public InstanceModel getInstance() {
        return instance;
    }

    public int[] getObjectiveValues() {
        return objectiveValues;
    }
}
