package org.clafer.compiler;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.search.loop.monitors.SMF;

/**
 * Search for instances.
 *
 * @author jimmy
 */
public interface ClaferSearch extends InstanceIterator {

    /**
     * {@inheritDoc}
     *
     * @throws ReachedLimitException if resource limit reached
     */
    @Override
    public boolean find() throws ReachedLimitException;

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
