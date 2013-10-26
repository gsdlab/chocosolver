package org.clafer.collection;

import java.util.Arrays;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class PairTest {

    private static Pair<Integer, Integer> pair(Integer fst, Integer snd) {
        return new Pair<Integer, Integer>(fst, snd);
    }

    @Test
    public void testGetFst() {
        assertEquals(1, pair(1, 2).getFst().intValue());
    }

    @Test
    public void testGetSnd() {
        assertEquals(2, pair(1, 2).getSnd().intValue());
    }

    @Test
    public void testMapFst() {
        @SuppressWarnings("unchecked")
        Pair<Integer, Integer>[] pairs = new Pair[]{pair(1, 3), pair(2, 6), pair(3, 9)};
        Integer[] fsts = new Integer[]{1, 2, 3};
        assertArrayEquals(fsts, Pair.mapFst(pairs));
        assertEquals(Arrays.asList(fsts), Pair.mapFst(Arrays.asList(pairs)));
    }

    @Test
    public void testMapSnd() {
        @SuppressWarnings("unchecked")
        Pair<Integer, Integer>[] pairs = new Pair[]{pair(1, 3), pair(2, 6), pair(3, 9)};
        Integer[] fsts = new Integer[]{3, 6, 9};
        assertArrayEquals(fsts, Pair.mapSnd(pairs));
        assertEquals(Arrays.asList(fsts), Pair.mapSnd(Arrays.asList(pairs)));
    }

    @Test
    public void testEquals() {
        assertEquals(pair(1, 2), pair(1, 2));
        assertNotEquals(pair(1, 2), pair(2, 1));
    }

    @Test
    public void testHashCode() {
        assertEquals(pair(1, 2).hashCode(), pair(1, 2).hashCode());
    }

    @Test
    public void testString() {
        assertEquals("(1, 2)", pair(1, 2).toString());
    }
}
