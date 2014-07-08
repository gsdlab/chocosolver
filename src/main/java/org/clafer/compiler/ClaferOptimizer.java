package org.clafer.compiler;

/**
 *
 * @author jimmy
 */
public interface ClaferOptimizer extends ClaferSearch {

    /**
     * Returns the optimal values. The order of the values corresponds to the
     * order of the objectives.
     *
     * @return the optimal value
     * @exception IllegalStateException if no solution have been found yet
     */
    int[] optimalValues();
}
