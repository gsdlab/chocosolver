package org.clafer.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

/**
 *
 * @param <V> the type of the data
 * @author jimmy
 */
public class TopologicalSort<V> {

    private int index = 0;
    private Map<Vertex<V>, Index> vertexIndices = new HashMap<Vertex<V>, Index>();
    private Stack<Vertex<V>> S = new Stack<Vertex<V>>();
    private List<Set<V>> components = new ArrayList<Set<V>>();

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
        TopologicalSort<V> tarjan = new TopologicalSort<V>();
        for (Vertex<V> vertex : graph.getVertices()) {
            if (!tarjan.vertexIndices.containsKey(vertex)) {
                tarjan.strongConnect(vertex);
            }
        }
        return tarjan.components;
    }

    private Index strongConnect(Vertex<V> vertex) {
        Index vertexIndex = new Index(index, index);
        vertexIndices.put(vertex, vertexIndex);
        index++;

        S.push(vertex);

        for (Vertex<V> neighbour : vertex.getNeighbours()) {
            Index neighbourIndex = vertexIndices.get(neighbour);
            if (neighbourIndex == null) {
                neighbourIndex = strongConnect(neighbour);
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

        public int getIndex() {
            return index;
        }

        public void setIndex(int index) {
            this.index = index;
        }

        public int getLowIndex() {
            return lowIndex;
        }

        public void setLowIndex(int lowIndex) {
            this.lowIndex = lowIndex;
        }

        public void setLowIndexMin(int lowIndex) {
            if (this.lowIndex >= lowIndex) {
                this.lowIndex = lowIndex;
            }
        }
    }
}
