package org.clafer.graph;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TopologicalSortTest {

    private static <T> HashSet<T> set(T... items) {
        return new HashSet<T>(Arrays.asList(items));
    }

    @Test
    public void testSingletonNodes() {
        KeyGraph<Character> graph = new KeyGraph<Character>();
        graph.getVertex('a');
        graph.getVertex('b');

        List<Set<Character>> components = GraphUtil.computeStronglyConnectedComponents(graph);

        assertEquals(2, components.size());
    }

    @Test
    public void testSelfReference() {
        KeyGraph<Character> graph = new KeyGraph<Character>();
        graph.getVertex('a').addNeighbour(graph.getVertex('b'));
        graph.getVertex('b').addNeighbour(graph.getVertex('b'));
        graph.getVertex('b').addNeighbour(graph.getVertex('c'));

        List<Set<Character>> components = GraphUtil.computeStronglyConnectedComponents(graph);

        assertEquals(3, components.size());

        assertEquals(set('c'), components.get(0));
        assertEquals(set('b'), components.get(1));
        assertEquals(set('a'), components.get(2));
    }

    @Test
    public void testCycles() {
        KeyGraph<Character> graph = new KeyGraph<Character>();
        graph.getVertex('a').addNeighbour(graph.getVertex('b'));
        graph.getVertex('b').addNeighbour(graph.getVertex('c'));
        graph.getVertex('c').addNeighbour(graph.getVertex('d'));
        graph.getVertex('d').addNeighbour(graph.getVertex('e'));
        graph.getVertex('d').addNeighbour(graph.getVertex('b'));
        graph.getVertex('f').addNeighbour(graph.getVertex('b'));
        graph.getVertex('g').addNeighbour(graph.getVertex('f'));
        graph.getVertex('h').addNeighbour(graph.getVertex('b'));
        graph.getVertex('i').addNeighbour(graph.getVertex('a'));
        graph.getVertex('j').addNeighbour(graph.getVertex('i'));
        graph.getVertex('j').addNeighbour(graph.getVertex('g'));
        graph.getVertex('j').addNeighbour(graph.getVertex('k'));
        graph.getVertex('i').addNeighbour(graph.getVertex('g'));
        graph.getVertex('f').addNeighbour(graph.getVertex('j'));
        graph.getVertex('e').addNeighbour(graph.getVertex('e'));
        graph.getVertex('b').addNeighbour(graph.getVertex('b'));
        graph.getVertex('k').addNeighbour(graph.getVertex('a'));

        List<Set<Character>> components = GraphUtil.computeStronglyConnectedComponents(graph);

        assertEquals(6, components.size());

        assertEquals(set('e'), components.get(0));
        assertEquals(set('b', 'c', 'd'), components.get(1));
        assertEquals(set('a'), components.get(2));
        assertEquals(set('k'), components.get(3));
        assertEquals(set('f', 'g', 'i', 'j'), components.get(4));
        assertEquals(set('h'), components.get(5));
    }
}
