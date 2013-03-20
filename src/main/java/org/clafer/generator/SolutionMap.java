package org.clafer.generator;

/**
 *
 * @author jimmy
 */
interface SolutionMap<A, C> {

    boolean has(A a);

    C get(A a);
}
