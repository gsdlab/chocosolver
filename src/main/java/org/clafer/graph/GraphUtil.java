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

    private static <V> void findPath(Vertex<V> cur, Graph<V> graph, Set<Vertex<V>> visited) {
        if (!visited.add(cur)) {
            return;
        }
        for (Vertex<V> neighbour : cur.getNeighbours()) {
            findPath(neighbour, graph, visited);
        }
    }

    /**
     * Checks if there exists a path in the graph from the start node to the end
     * node.
     *
     * @param <V> the type of the data
     * @param start the start node
     * @param end the end node
     * @param graph the directed graph
     * @return {@code true} if there is a path in the graph from the start node
     * to the end node, {@code false} otherwise
     */
    public static <V> boolean hasPath(Vertex<V> start, Vertex<V> end, Graph<V> graph) {
        Set<Vertex<V>> visited = new HashSet<>();
        return findPath(start, end, graph, visited);
    }

    /**
     * Compute all the reachable nodes from the set of start nodes.
     *
     * @param <V> the type of the data
     * @param start the start nodes
     * @param graph the directed graph
     * @return all the reachable nodes from the set of start nodes
     */
    public static <V> Set<V> reachable(Set<Vertex<V>> start, Graph<V> graph) {
        Set<Vertex<V>> visited = new HashSet<>();
        for (Vertex<V> vertex : start) {
            findPath(vertex, graph, visited);
        }
        Set<V> reachable = new HashSet<>(visited.size());
        for (Vertex<V> visit : visited) {
            reachable.add(visit.getData());
        }
        return reachable;
    }

    /**
     * Compute the strongly connected components in the graph in topological
     * order. Implementation of Tarjan's algorithm.
     *
     * @param <V> the type of the data
     * @param graph the directed graph
     * @return the strongly connected components in topological order.
     * @see <a
     * href="http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm">Tarjan's
     * algorithm</a>
     */
    public static <V> List<Set<V>> computeStronglyConnectedComponents(Graph<V> graph) {
        Counter counter = new Counter();
        Map<Vertex<V>, Index> vertexIndices = new HashMap<>();
        Stack<Vertex<V>> S = new Stack<>();
        List<Set<V>> components = new ArrayList<>();

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
            Set<V> component = new HashSet<>();

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

        private final int index;
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
