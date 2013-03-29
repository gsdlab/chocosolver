package org.clafer.collection;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author jimmy
 */
public class DirectedGraphBuilder<V> {

    private final Map<V, DirectedVertex<V>> vertices = new HashMap<V, DirectedVertex<V>>();

    public DirectedVertex<V> getVertex(V data) {
        DirectedVertex<V> vertex = vertices.get(data);
        if (vertex == null) {
            vertex = new DirectedVertex<V>(data);
            vertices.put(data, vertex);
        }
        return vertex;
    }

    public DirectedGraph<V> toGraph() {
        return new DirectedGraph<V>(vertices.values());
    }
}
