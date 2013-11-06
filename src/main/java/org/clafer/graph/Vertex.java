package org.clafer.graph;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.clafer.common.Check;

/**
 * A mutable directed vertex.
 *
 * @param <V> the type of the data
 * @author jimmy
 */
public class Vertex<V> {

    private final V data;
    private final Set<Vertex<V>> neighbours = new HashSet<>();

    public Vertex(V data) {
        this.data = Check.notNull(data);
    }

    /**
     * Returns the data associated with this vertex. Two two vertices with the
     * same data are still different vertices.
     *
     * @return the data associated with this vertex
     */
    public V getData() {
        return data;
    }

    /**
     * Returns the set of vertexes that have an incoming edge from this vertex.
     *
     * @return the neighbours of this vertex
     */
    public Set<Vertex<V>> getNeighbours() {
        return Collections.unmodifiableSet(neighbours);
    }

    /**
     * Add a new edge starting from this edge to the neighbour. If this edge
     * already exists, then no changes occur.
     *
     * @param neighbour a new neighbour of this vertex
     * @return this vertex
     */
    public Vertex addNeighbour(Vertex<V> neighbour) {
        neighbours.add(neighbour);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return data.hashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return data.toString();
    }
}
