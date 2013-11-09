package org.clafer.common;

import java.util.Arrays;
import java.util.List;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class UtilTest {

    private static Integer[] array(Integer... items) {
        return items;
    }

    private static List<Integer> list(Integer... items) {
        return Arrays.asList(items);
    }

    @Test
    public void testConcat() {
        assertArrayEquals(array(1, 2, 3, 3, 4, 5, 6, 6, 7),
                Util.concat(new Integer[][]{array(1, 2, 3), array(3, 4, 5, 6), array(6, 7)}));
        assertArrayEquals(array(1, 2, 3, 6, 7),
                Util.concat(new Integer[][]{array(1, 2, 3), array(), array(6, 7)}));
        assertArrayEquals(array(1, 2, 3, 3, 4, 5, 6),
                Util.concat(new Integer[][]{array(1, 2, 3), array(3, 4, 5, 6), array()}));
        assertArrayEquals(array(3, 4, 5, 6, 6, 7),
                Util.concat(new Integer[][]{array(), array(3, 4, 5, 6), array(6, 7)}));
        assertArrayEquals(array(),
                Util.concat(new Integer[][]{array(), array(), array()}));
        assertArrayEquals(array(), Util.concat(new Integer[][]{}));
    }

    @Test
    public void testPermutations() {
        assertTrue(Arrays.deepEquals(new Integer[][]{
            array(1, 2), array(1, 3), array(1, 4), array(2, 1), array(2, 3), array(2, 4),
            array(3, 1), array(3, 2), array(3, 4), array(4, 1), array(4, 2), array(4, 3)
        }, Util.permutations(array(1, 2, 3, 4), 2)));
        assertTrue(Arrays.deepEquals(new Integer[][]{array()}, Util.permutations(array(1, 2, 3, 4), 0)));
        assertTrue(Arrays.deepEquals(new Integer[][]{array()}, Util.permutations(array(), 0)));
    }

    @Test
    public void testSequence() {
        assertTrue(Arrays.deepEquals(new Integer[][]{
            array(1, 4, 8), array(1, 4, 9), array(1, 5, 8), array(1, 5, 9), array(1, 6, 8), array(1, 6, 9), array(1, 7, 8), array(1, 7, 9),
            array(2, 4, 8), array(2, 4, 9), array(2, 5, 8), array(2, 5, 9), array(2, 6, 8), array(2, 6, 9),
            array(2, 7, 8), array(2, 7, 9), array(3, 4, 8), array(3, 4, 9), array(3, 5, 8), array(3, 5, 9),
            array(3, 6, 8), array(3, 6, 9), array(3, 7, 8), array(3, 7, 9)
        }, Util.sequence(new Integer[][]{array(1, 2, 3), array(4, 5, 6, 7), array(8, 9)})));
        assertTrue(Arrays.deepEquals(new Integer[][]{},
                Util.sequence(new Integer[][]{array(1, 2, 3), array(4, 5, 6, 7), array(), array(8, 9)})));
        assertTrue(Arrays.deepEquals(new Integer[][]{array()},
                Util.sequence(new Integer[][]{})));
    }

    @Test
    public void testTakeUntil() {
        assertEquals(Util.takeUntil(1, list(1, 2, 3, 4, 2)), list(1));
        assertEquals(Util.takeUntil(2, list(1, 2, 3, 4, 2)), list(1, 2));
        assertEquals(Util.takeUntil(3, list(1, 2, 3, 4, 2)), list(1, 2, 3));
        assertEquals(Util.takeUntil(4, list(1, 2, 3, 4, 2)), list(1, 2, 3, 4));
        assertEquals(Util.takeUntil(5, list(1, 2, 3, 4, 2)), list(1, 2, 3, 4, 2));
    }

    @Test
    public void testDropUntil() {
        assertEquals(Util.dropUntil(1, list(1, 2, 3, 4, 2)), list(1, 2, 3, 4, 2));
        assertEquals(Util.dropUntil(2, list(1, 2, 3, 4, 2)), list(2, 3, 4, 2));
        assertEquals(Util.dropUntil(3, list(1, 2, 3, 4, 2)), list(3, 4, 2));
        assertEquals(Util.dropUntil(4, list(1, 2, 3, 4, 2)), list(4, 2));
        assertEquals(Util.dropUntil(5, list(1, 2, 3, 4, 2)), list());
    }

    @Test
    public void testStartsWith() {
        assertTrue(Util.startsWith(list(1, 2, 3, 4, 2), list()));
        assertTrue(Util.startsWith(list(1, 2, 3, 4, 2), list(1)));
        assertTrue(Util.startsWith(list(1, 2, 3, 4, 2), list(1, 2)));
        assertTrue(Util.startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3)));
        assertTrue(Util.startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4)));
        assertTrue(Util.startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 2)));

        assertFalse(Util.startsWith(list(1, 2, 3, 4, 2), list(2)));
        assertFalse(Util.startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 3)));
        assertFalse(Util.startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 2, 3)));
    }

    @Test
    public void testEndsWith() {
        assertTrue(Util.endsWith(list(1, 2, 3, 4, 2), list()));
        assertTrue(Util.endsWith(list(1, 2, 3, 4, 2), list(2)));
        assertTrue(Util.endsWith(list(1, 2, 3, 4, 2), list(4, 2)));
        assertTrue(Util.endsWith(list(1, 2, 3, 4, 2), list(3, 4, 2)));
        assertTrue(Util.endsWith(list(1, 2, 3, 4, 2), list(2, 3, 4, 2)));
        assertTrue(Util.endsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 2)));

        assertFalse(Util.endsWith(list(1, 2, 3, 4, 2), list(1)));
        assertFalse(Util.endsWith(list(1, 2, 3, 4, 2), list(2, 4, 2)));
        assertFalse(Util.endsWith(list(1, 2, 3, 4, 2), list(1, 1, 2, 3, 4, 2)));
    }
}
