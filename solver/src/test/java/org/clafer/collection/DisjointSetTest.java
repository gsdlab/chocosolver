package org.clafer.collection;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class DisjointSetTest {

    private static Collection<Set<Integer>> collection(Set<Integer>... values) {
        return new HashSet<>(Arrays.asList(values));
    }

    private static Set<Integer> set(Integer... values) {
        return new HashSet<>(Arrays.asList(values));
    }

    @Test
    public void testUnionConnected() {
        DisjointSets<Integer> sets = new DisjointSets<>();

        assertFalse(sets.connected(1, 2));
        assertFalse(sets.connected(2, 1));

        sets.union(1, 2);
        assertTrue(sets.connected(1, 2));
        assertTrue(sets.connected(2, 1));
        assertFalse(sets.connected(3, 4));

        sets.union(2, 1);
        assertTrue(sets.connected(1, 2));
        assertTrue(sets.connected(2, 1));
        assertFalse(sets.connected(3, 4));

        sets.union(3, 4);
        assertTrue(sets.connected(1, 2));
        assertTrue(sets.connected(2, 1));
        assertTrue(sets.connected(3, 4));
        assertTrue(sets.connected(4, 3));
        assertFalse(sets.connected(1, 3));
        assertFalse(sets.connected(1, 4));
        assertFalse(sets.connected(2, 3));
        assertFalse(sets.connected(2, 4));

        sets.union(1, 5);
        assertTrue(sets.connected(1, 2));
        assertTrue(sets.connected(2, 1));
        assertTrue(sets.connected(1, 5));
        assertTrue(sets.connected(5, 1));
        assertTrue(sets.connected(2, 5));
        assertTrue(sets.connected(5, 2));
        assertTrue(sets.connected(3, 4));
        assertTrue(sets.connected(4, 3));
        assertFalse(sets.connected(1, 3));
        assertFalse(sets.connected(1, 4));
        assertFalse(sets.connected(2, 3));
        assertFalse(sets.connected(2, 4));
        assertFalse(sets.connected(5, 3));
        assertFalse(sets.connected(5, 4));

        sets.union(1, 3);
        assertTrue(sets.connected(1, 2));
        assertTrue(sets.connected(2, 1));
        assertTrue(sets.connected(1, 3));
        assertTrue(sets.connected(3, 1));
        assertTrue(sets.connected(1, 4));
        assertTrue(sets.connected(4, 1));
        assertTrue(sets.connected(1, 5));
        assertTrue(sets.connected(5, 1));
        assertTrue(sets.connected(2, 3));
        assertTrue(sets.connected(3, 2));
        assertTrue(sets.connected(2, 4));
        assertTrue(sets.connected(4, 2));
        assertTrue(sets.connected(2, 5));
        assertTrue(sets.connected(5, 2));
        assertTrue(sets.connected(3, 4));
        assertTrue(sets.connected(4, 3));
        assertTrue(sets.connected(3, 5));
        assertTrue(sets.connected(5, 3));
        assertTrue(sets.connected(4, 5));
        assertTrue(sets.connected(5, 4));
        assertFalse(sets.connected(1, 6));
        assertFalse(sets.connected(6, 1));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testConnectedComponents() {
        DisjointSets<Integer> sets = new DisjointSets<>();

        assertEquals(collection(), sets.connectedComponents());

        sets.union(1, 2);
        assertEquals(collection(set(1, 2)), sets.connectedComponents());

        sets.union(2, 1);
        assertEquals(collection(set(1, 2)), sets.connectedComponents());

        sets.union(3, 4);
        assertEquals(collection(set(1, 2), set(3, 4)), sets.connectedComponents());

        sets.union(1, 5);
        assertEquals(collection(set(1, 2, 5), set(3, 4)), sets.connectedComponents());

        sets.union(1, 3);
        assertEquals(collection(set(1, 2, 3, 4, 5)), sets.connectedComponents());
    }
}
