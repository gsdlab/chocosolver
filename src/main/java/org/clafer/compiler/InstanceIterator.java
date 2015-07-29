package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.instance.InstanceModel;

/**
 *
 * @author jimmy
 */
public interface InstanceIterator {

    /**
     * Find the next instance.
     *
     * @return {@code true} if and only if another solution is found,
     * {@code false} otherwise
     * @throws ReachedLimitException if resource limit reached
     */
    public boolean find();

    /**
     * Return the instance from the last {@link #find()} operation, if
     * successful.
     *
     * @return the instance
     */
    public InstanceModel instance();

    /**
     * Return all the remaining instances.
     *
     * @return all the remaining instances
     * @throws ReachedLimitException if resource limit reached
     */
    public default InstanceModel[] allInstances() throws ReachedLimitException {
        List<InstanceModel> instances = new ArrayList<>();
        while (find()) {
            instances.add(instance());
        }
        return instances.toArray(new InstanceModel[instances.size()]);
    }

    /**
     * Return the number of instances found so far.
     *
     * @return the number of instances found so far
     */
    public int instanceCount();
}
