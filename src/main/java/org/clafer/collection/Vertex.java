package org.clafer.collection;

import java.util.HashSet;
import java.util.Set;
import org.clafer.Check;

/**
 * Directed vertex.
 * 
 * @author jimmy
 */
public class Vertex<V> {

    private final V data;
    private final Set<Vertex<V>> neighbours = new HashSet<Vertex<V>>();

    public Vertex(V data) {
        this.data = Check.notNull(data);
    }

    public V getData() {
        return data;
    }

    public Set<Vertex<V>> getNeighbours() {
        return neighbours;
    }

    public Vertex addNeighbour(Vertex<V> neighbour) {
        neighbours.add(neighbour);
        return this;
    }

//    @Override
//    public int hashCode() {
//        return data.hashCode();
//    }
//
//    @Override
//    public boolean equals(Object obj) {
//        if (obj instanceof DirectedVertex) {
//            DirectedVertex o = (DirectedVertex<V>) obj;
//            return data.equals(o.data);
//        }
//        return false;
//    }

    @Override
    public String toString() {
        return data.toString();
    }
}
