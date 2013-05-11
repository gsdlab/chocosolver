package org.clafer.collection;

import java.util.HashSet;
import java.util.Set;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class DirectedVertex<V> {

    private final V data;
    private final Set<DirectedVertex<V>> neighbours = new HashSet<DirectedVertex<V>>();

    public DirectedVertex(V data) {
        this.data = Check.notNull(data);
    }

    public V getData() {
        return data;
    }

    public Set<DirectedVertex<V>> getNeighbours() {
        return neighbours;
    }

    public DirectedVertex addNeighbour(DirectedVertex<V> neighbour) {
        neighbours.add(neighbour);
        return this;
    }

    @Override
    public int hashCode() {
        return data.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DirectedVertex) {
            DirectedVertex o = (DirectedVertex<V>) obj;
            return data.equals(o.data);
        }
        return false;
    }

    @Override
    public String toString() {
        return data.toString();
    }
}
