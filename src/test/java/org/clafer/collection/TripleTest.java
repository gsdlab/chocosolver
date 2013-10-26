package org.clafer.collection;

import java.util.Arrays;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TripleTest {

    private static Pair<Integer, Integer> pair(Integer fst, Integer snd) {
        return new Pair<Integer, Integer>(fst, snd);
    }

    private static Triple<Integer, Integer, Integer> triple(Integer fst, Integer snd, Integer thd) {
        return new Triple<Integer, Integer, Integer>(fst, snd, thd);
    }

    private static Triple<Integer, Integer, Integer> triple(Integer fst, Pair<Integer, Integer> sndThd) {
        return new Triple<Integer, Integer, Integer>(fst, sndThd);
    }

    private static Triple<Integer, Integer, Integer> triple(Pair<Integer, Integer> fstSnd, Integer thd) {
        return new Triple<Integer, Integer, Integer>(fstSnd, thd);
    }

    @Test
    public void testGetFst() {
        assertEquals(1, triple(1, 2, 3).getFst().intValue());
        assertEquals(1, triple(pair(1, 2), 3).getFst().intValue());
        assertEquals(1, triple(1, pair(2, 3)).getFst().intValue());
    }

    @Test
    public void testGetSnd() {
        assertEquals(2, triple(1, 2, 3).getSnd().intValue());
        assertEquals(2, triple(pair(1, 2), 3).getSnd().intValue());
        assertEquals(2, triple(1, pair(2, 3)).getSnd().intValue());
    }

    @Test
    public void testGetThd() {
        assertEquals(3, triple(1, 2, 3).getThd().intValue());
        assertEquals(3, triple(pair(1, 2), 3).getThd().intValue());
        assertEquals(3, triple(1, pair(2, 3)).getThd().intValue());
    }

    @Test
    public void testGetFstSnd() {
        assertEquals(pair(1, 2), triple(1, 2, 3).getFstSnd());
        assertEquals(pair(1, 2), triple(pair(1, 2), 3).getFstSnd());
        assertEquals(pair(1, 2), triple(1, pair(2, 3)).getFstSnd());
    }

    @Test
    public void testGetSndThd() {
        assertEquals(pair(2, 3), triple(1, 2, 3).getSndThd());
        assertEquals(pair(2, 3), triple(pair(1, 2), 3).getSndThd());
        assertEquals(pair(2, 3), triple(1, pair(2, 3)).getSndThd());
    }

    @Test
    public void testMapFst() {
        @SuppressWarnings("unchecked")
        Triple<Integer, Integer, Integer>[] triples = new Triple[]{
            triple(1, 3, 10), triple(2, 6, 9), triple(3, 9, 8)};
        Integer[] fsts = new Integer[]{1, 2, 3};
        assertArrayEquals(fsts, Triple.mapFst(triples));
        assertEquals(Arrays.asList(fsts), Triple.mapFst(Arrays.asList(triples)));
    }

    @Test
    public void testMapSnd() {
        @SuppressWarnings("unchecked")
        Triple<Integer, Integer, Integer>[] triples = new Triple[]{
            triple(1, 3, 10), triple(2, 6, 9), triple(3, 9, 8)};
        Integer[] fsts = new Integer[]{3, 6, 9};
        assertArrayEquals(fsts, Triple.mapSnd(triples));
        assertEquals(Arrays.asList(fsts), Triple.mapSnd(Arrays.asList(triples)));
    }

    @Test
    public void testMapThd() {
        @SuppressWarnings("unchecked")
        Triple<Integer, Integer, Integer>[] triples = new Triple[]{
            triple(1, 3, 10), triple(2, 6, 9), triple(3, 9, 8)};
        Integer[] fsts = new Integer[]{10, 9, 8};
        assertArrayEquals(fsts, Triple.mapThd(triples));
        assertEquals(Arrays.asList(fsts), Triple.mapThd(Arrays.asList(triples)));
    }

    @Test
    public void testEquals() {
        assertEquals(triple(1, 2, 3), triple(pair(1, 2), 3));
        assertEquals(triple(1, pair(2, 3)), triple(pair(1, 2), 3));
        assertNotEquals(triple(3, pair(1, 2)), triple(pair(1, 2), 3));
    }

    @Test
    public void testHashCode() {
        assertEquals(triple(1, 2, 3).hashCode(), triple(pair(1, 2), 3).hashCode());
        assertEquals(triple(1, pair(2, 3)).hashCode(), triple(pair(1, 2), 3).hashCode());
    }

    @Test
    public void testToString() {
        assertEquals("(1, 2, 3)", triple(1, 2, 3).toString());
        assertEquals("(1, 2, 3)", triple(pair(1, 2), 3).toString());
        assertEquals("(1, 2, 3)", triple(1, pair(2, 3)).toString());
    }
}
