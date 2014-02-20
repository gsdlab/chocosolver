package org.clafer.collection;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class PairTest {

    @SafeVarargs
    private static Pair<Integer, Integer>[] pairs(Pair<Integer, Integer>... pairs) {
        return pairs;
    }

    private static Pair<Integer, Integer> pair(Integer fst, Integer snd) {
        return new Pair<>(fst, snd);
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
        Pair<Integer, Integer>[] pairs = pairs(pair(1, 3), pair(2, 6), pair(3, 9));
        Integer[] fsts = new Integer[]{1, 2, 3};
        assertArrayEquals(fsts, Pair.mapFst(pairs));
        assertEquals(Arrays.asList(fsts), Pair.mapFst(Arrays.asList(pairs)));
    }

    @Test
    public void testMapSnd() {
        Pair<Integer, Integer>[] pairs = pairs(pair(1, 3), pair(2, 6), pair(3, 9));
        Integer[] fsts = new Integer[]{3, 6, 9};
        assertArrayEquals(fsts, Pair.mapSnd(pairs));
        assertEquals(Arrays.asList(fsts), Pair.mapSnd(Arrays.asList(pairs)));
    }

    @Test
    public void testFromPairs() {
        Map<Integer, Integer> map = new HashMap<>();
        map.put(1, 4);
        map.put(2, 6);
        map.put(3, 9);
        assertEquals(
                Pair.fromPairs(pairs(pair(1, 3), pair(2, 6), pair(3, 9), pair(1, 4))),
                map);
    }

    @Test
    public void testToPairs() {
        Map<Integer, Integer> map = new HashMap<>();
        map.put(1, 3);
        map.put(2, 6);
        map.put(3, 9);
        Pair<Integer, Integer>[] pairs = Pair.toPairs(map);
        Arrays.sort(pairs, new Comparator<Pair<Integer, Integer>>() {

            @Override
            public int compare(Pair<Integer, Integer> o1, Pair<Integer, Integer> o2) {
                return Integer.compare(o1.getFst(), o2.getFst());
            }
        });
        assertArrayEquals(
                pairs(pair(1, 3), pair(2, 6), pair(3, 9)),
                pairs);
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
