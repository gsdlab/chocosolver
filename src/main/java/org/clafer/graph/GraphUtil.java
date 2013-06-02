package org.clafer.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import org.clafer.collection.Counter;

/**
 *
 * @author jimmy
 */
public class GraphUtil {

    private GraphUtil() {
    }

    public static <V> boolean hasPath(Vertex<V> start, Vertex<V> end, Graph<V> graph) {
        Set<Vertex<V>> visited = new HashSet<Vertex<V>>();
        return findPath(start, end, graph, visited);
    }

    private static <V> boolean findPath(Vertex<V> cur, Vertex<V> end, Graph<V> graph, Set<Vertex<V>> visited) {
        if (cur.equals(end)) {
            return true;
        }
        if (!visited.add(cur)) {
            return false;
        }
        for (Vertex<V> neighbour : cur.getNeighbours()) {
            if (findPath(neighbour, end, graph, visited)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Compute the strongly connected components in the graph in topological
     * order. Implementation of Tarjan's algorithm.
     *
     * @param <V>
     * @param graph
     * @return the strongly connected components in topological order.
     * @see <a
     * href="http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm">Tarjan's
     * algorithm</a>
     */
    public static <V> List<Set<V>> computeStronglyConnectedComponents(Graph<V> graph) {
        Counter counter = new Counter();
        Map<Vertex<V>, Index> vertexIndices = new HashMap<Vertex<V>, Index>();
        Stack<Vertex<V>> S = new Stack<Vertex<V>>();
        List<Set<V>> components = new ArrayList<Set<V>>();

        for (Vertex<V> vertex : graph.getVertices()) {
            if (!vertexIndices.containsKey(vertex)) {
                strongConnect(vertex, counter, vertexIndices, S, components);
            }
        }
        return components;
    }

    private static <V> Index strongConnect(Vertex<V> vertex, Counter counter, Map<Vertex<V>, Index> vertexIndices,
            Stack<Vertex<V>> S, List<Set<V>> components) {
        int index = counter.next();
        Index vertexIndex = new Index(index, index);
        vertexIndices.put(vertex, vertexIndex);

        S.push(vertex);

        for (Vertex<V> neighbour : vertex.getNeighbours()) {
            Index neighbourIndex = vertexIndices.get(neighbour);
            if (neighbourIndex == null) {
                neighbourIndex = strongConnect(neighbour, counter, vertexIndices, S, components);
                vertexIndex.setLowIndexMin(neighbourIndex.getLowIndex());
            } else if (S.contains(neighbour)) {
                vertexIndex.setLowIndexMin(neighbourIndex.getIndex());
            }
        }

        if (vertexIndex.getLowIndex() == vertexIndex.getIndex()) {
            Set<V> component = new HashSet<V>();

            Vertex<V> cycle;
            do {
                cycle = S.pop();
                component.add(cycle.getData());
            } while (cycle != vertex);

            components.add(component);
        }
        return vertexIndex;
    }

    private static class Index {

        private int index;
        private int lowIndex;

        Index(int index, int lowIndex) {
            this.index = index;
            this.lowIndex = lowIndex;
        }

        int getIndex() {
            return index;
        }

        int getLowIndex() {
            return lowIndex;
        }

        void setLowIndexMin(int lowIndex) {
            if (this.lowIndex >= lowIndex) {
                this.lowIndex = lowIndex;
            }
        }
    }
}
