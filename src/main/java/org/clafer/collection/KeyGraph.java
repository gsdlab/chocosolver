package org.clafer.collection;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * There exists a bijection between the data and the vertices.
 * 
 * @author jimmy
 */
public class KeyGraph<V> implements Graph<V> {

    private final Map<V, Vertex<V>> vertices = new HashMap<V, Vertex<V>>();

    public Vertex<V> getVertex(V data) {
        Vertex<V> vertex = vertices.get(data);
        if (vertex == null) {
            vertex = new Vertex<V>(data);
            vertices.put(data, vertex);
        }
        return vertex;
    }

    @Override
    public Collection<Vertex<V>> getVertices() {
        return vertices.values();
    }
}
