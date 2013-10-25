package org.clafer.compiler;

import solver.Solver;

/**
 *
 * @param <T> type of the instance
 * @author jimmy
 */
public interface ClaferSearch<T> {

    public boolean find();

    public T instance();

    public T[] allInstances();

    public Solver getInternalSolver();
}
