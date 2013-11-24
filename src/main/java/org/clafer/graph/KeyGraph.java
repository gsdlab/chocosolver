package org.clafer.graph;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * A graph where every data is mapped to exactly one node.
 * <p>
 * There exists a injection from the data to the vertices. This graph maintains
 * a mapping from data to its vertex. New vertices are created on-the-fly.
 * </p>
 * <p>For example, a 2-cycle graph:
 * <pre>
 * KeyGraph&lt;Character&gt;graph = new KeyGraph&lt;Character&gt;();
 * graph.getVertex('a').addNeighbour(graph.getVertex('b'));
 * graph.getVertex('b').addNeighbour(graph.getVertex('a'));
 * </pre>
 * </p>
 *
 * @param <V> the type of the data
 * @author jimmy
 */
public class KeyGraph<V> implements Graph<V> {

    private final Map<V, Vertex<V>> vertices = new HashMap<>();

    /**
     * Returns the vertex associated with the data. The data class should
     * implement equals and hashCode.
     *
     * @param data the data
     * @return the vertex containing the data
     * @see Object#equals(Object)
     * @see Object#hashCode()
     */
    public Vertex<V> getVertex(V data) {
        Vertex<V> vertex = vertices.get(data);
        if (vertex == null) {
            vertex = new Vertex<>(data);
            vertices.put(data, vertex);
        }
        return vertex;
    }

    public Vertex<V> getVertexIfPresent(V data) {
        return vertices.get(data);
    }

    public void addEdge(V from, V to) {
        getVertex(from).addNeighbour(getVertex(to));
    }

    public void addUndirectedEdge(V from, V to) {
        Vertex<V> fromV = getVertex(from);
        Vertex<V> toV = getVertex(to);
        fromV.addNeighbour(toV);
        toV.addNeighbour(fromV);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<Vertex<V>> getVertices() {
        return vertices.values();
    }

    @Override
    public String toString() {
        return vertices.toString();
    }
}
