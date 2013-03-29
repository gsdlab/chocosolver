package org.clafer.collection;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author jimmy
 */
public class DirectedGraph<V> {

    private final Set<DirectedVertex<V>> vertices;

    public DirectedGraph(Collection<DirectedVertex<V>> vertices) {
        this.vertices = new HashSet<DirectedVertex<V>>(vertices);
    }

    public Set<DirectedVertex<V>> getVertices() {
        return vertices;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (DirectedVertex<V> vertex : vertices) {
            result.append(vertex.getData()).append("-->").append(vertex.getNeighbours()).append('\n');
        }
        return result.toString();
    }
}
