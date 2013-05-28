package org.clafer.collection;

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
