package org.clafer.graph;

import java.util.Collection;

/**
 * A mutable directed graph.
 *
 * @param <V> the type of the data
 * @author jimmy
 */
public interface Graph<V> {

    /**
     * Return the set of vertices contained in the graph. Vertices can be
     * removed but not added. Vertices can be mutated.
     *
     * @return the set of vertices
     */
    public Collection<Vertex<V>> getVertices();
}
