package org.clafer.collection;

import java.util.Collection;

/**
 * Directed graph.
 * 
 * @author jimmy
 */
public interface Graph<V> {

    public Collection<Vertex<V>> getVertices();
}
