package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.search.loop.monitors.SMF;

/**
 * Search for instances.
 *
 * @author jimmy
 */
public interface ClaferSearch {

    /**
     * Find the next instance.
     *
     * @return {@code true} if and only if another solution is found,
     * {@code false} otherwise
     * @throws ReachedLimitException if resource limit reached
     */
    public boolean find() throws ReachedLimitException;

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

    public default ClaferSearch limitTime(long ms) {
        SMF.limitTime(getInternalSolver(), ms);
        return this;
    }

    /**
     * Returns the internal Choco solver. For debugging purposes only.
     *
     * @return the internal Choco solver
     */
    public Solver getInternalSolver();
}
