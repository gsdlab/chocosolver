package org.clafer.common;

import java.util.Arrays;
import java.util.List;
import static org.clafer.common.Util.concat;
import static org.clafer.common.Util.divCeil;
import static org.clafer.common.Util.divFloor;
import static org.clafer.common.Util.dropUntil;
import static org.clafer.common.Util.endsWith;
import static org.clafer.common.Util.permutations;
import static org.clafer.common.Util.sequence;
import static org.clafer.common.Util.startsWith;
import static org.clafer.common.Util.takeUntil;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
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
                concat(new Integer[][]{array(1, 2, 3), array(3, 4, 5, 6), array(6, 7)}));
        assertArrayEquals(array(1, 2, 3, 6, 7),
                concat(new Integer[][]{array(1, 2, 3), array(), array(6, 7)}));
        assertArrayEquals(array(1, 2, 3, 3, 4, 5, 6),
                concat(new Integer[][]{array(1, 2, 3), array(3, 4, 5, 6), array()}));
        assertArrayEquals(array(3, 4, 5, 6, 6, 7),
                concat(new Integer[][]{array(), array(3, 4, 5, 6), array(6, 7)}));
        assertArrayEquals(array(),
                concat(new Integer[][]{array(), array(), array()}));
        assertArrayEquals(array(), concat(new Integer[][]{}));
    }

    @Test
    public void testPermutations() {
        assertTrue(Arrays.deepEquals(new Integer[][]{
            array(1, 2), array(1, 3), array(1, 4), array(2, 1), array(2, 3), array(2, 4),
            array(3, 1), array(3, 2), array(3, 4), array(4, 1), array(4, 2), array(4, 3)
        }, permutations(array(1, 2, 3, 4), 2)));
        assertTrue(Arrays.deepEquals(new Integer[][]{array()}, permutations(array(1, 2, 3, 4), 0)));
        assertTrue(Arrays.deepEquals(new Integer[][]{array()}, permutations(array(), 0)));
    }

    @Test
    public void testSequence() {
        assertTrue(Arrays.deepEquals(new Integer[][]{
            array(1, 4, 8), array(1, 4, 9), array(1, 5, 8), array(1, 5, 9), array(1, 6, 8), array(1, 6, 9), array(1, 7, 8), array(1, 7, 9),
            array(2, 4, 8), array(2, 4, 9), array(2, 5, 8), array(2, 5, 9), array(2, 6, 8), array(2, 6, 9),
            array(2, 7, 8), array(2, 7, 9), array(3, 4, 8), array(3, 4, 9), array(3, 5, 8), array(3, 5, 9),
            array(3, 6, 8), array(3, 6, 9), array(3, 7, 8), array(3, 7, 9)
        }, sequence(new Integer[][]{array(1, 2, 3), array(4, 5, 6, 7), array(8, 9)})));
        assertTrue(Arrays.deepEquals(new Integer[][]{},
                sequence(new Integer[][]{array(1, 2, 3), array(4, 5, 6, 7), array(), array(8, 9)})));
        assertTrue(Arrays.deepEquals(new Integer[][]{array()},
                sequence(new Integer[][]{})));
    }

    @Test
    public void testTakeUntil() {
        assertEquals(takeUntil(1, list(1, 2, 3, 4, 2)), list(1));
        assertEquals(takeUntil(2, list(1, 2, 3, 4, 2)), list(1, 2));
        assertEquals(takeUntil(3, list(1, 2, 3, 4, 2)), list(1, 2, 3));
        assertEquals(takeUntil(4, list(1, 2, 3, 4, 2)), list(1, 2, 3, 4));
        assertEquals(takeUntil(5, list(1, 2, 3, 4, 2)), list(1, 2, 3, 4, 2));
    }

    @Test
    public void testDropUntil() {
        assertEquals(dropUntil(1, list(1, 2, 3, 4, 2)), list(1, 2, 3, 4, 2));
        assertEquals(dropUntil(2, list(1, 2, 3, 4, 2)), list(2, 3, 4, 2));
        assertEquals(dropUntil(3, list(1, 2, 3, 4, 2)), list(3, 4, 2));
        assertEquals(dropUntil(4, list(1, 2, 3, 4, 2)), list(4, 2));
        assertEquals(dropUntil(5, list(1, 2, 3, 4, 2)), list());
    }

    @Test
    public void testStartsWith() {
        assertTrue(startsWith(list(1, 2, 3, 4, 2), list()));
        assertTrue(startsWith(list(1, 2, 3, 4, 2), list(1)));
        assertTrue(startsWith(list(1, 2, 3, 4, 2), list(1, 2)));
        assertTrue(startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3)));
        assertTrue(startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4)));
        assertTrue(startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 2)));

        assertFalse(startsWith(list(1, 2, 3, 4, 2), list(2)));
        assertFalse(startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 3)));
        assertFalse(startsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 2, 3)));
    }

    @Test
    public void testEndsWith() {
        assertTrue(endsWith(list(1, 2, 3, 4, 2), list()));
        assertTrue(endsWith(list(1, 2, 3, 4, 2), list(2)));
        assertTrue(endsWith(list(1, 2, 3, 4, 2), list(4, 2)));
        assertTrue(endsWith(list(1, 2, 3, 4, 2), list(3, 4, 2)));
        assertTrue(endsWith(list(1, 2, 3, 4, 2), list(2, 3, 4, 2)));
        assertTrue(endsWith(list(1, 2, 3, 4, 2), list(1, 2, 3, 4, 2)));

        assertFalse(endsWith(list(1, 2, 3, 4, 2), list(1)));
        assertFalse(endsWith(list(1, 2, 3, 4, 2), list(2, 4, 2)));
        assertFalse(endsWith(list(1, 2, 3, 4, 2), list(1, 1, 2, 3, 4, 2)));
    }

    @Test
    public void testDivFloor() {
        assertEquals(4, divFloor(16, 4));
        assertEquals(4, divFloor(17, 4));
        assertEquals(4, divFloor(18, 4));
        assertEquals(4, divFloor(19, 4));
        assertEquals(-4, divFloor(-16, 4));
        assertEquals(-5, divFloor(-17, 4));
        assertEquals(-5, divFloor(-18, 4));
        assertEquals(-5, divFloor(-19, 4));
    }

    @Test(expected = ArithmeticException.class)
    public void testDivZeroFloor() {
        divFloor(16, 0);
    }

    @Test
    public void testDivCeil() {
        assertEquals(4, divCeil(16, 4));
        assertEquals(5, divCeil(17, 4));
        assertEquals(5, divCeil(18, 4));
        assertEquals(5, divCeil(19, 4));
        assertEquals(-4, divCeil(-16, 4));
        assertEquals(-4, divCeil(-17, 4));
        assertEquals(-4, divCeil(-18, 4));
        assertEquals(-4, divCeil(-19, 4));
    }

    @Test(expected = ArithmeticException.class)
    public void testDivZeroCeil() {
        divCeil(16, 0);
    }

    @Test
    public void testDowncast() {
        Number[] numbers = new Number[]{1, 2};
        Integer[] integers = Util.<Integer>cast(numbers);
        assertEquals(2, integers.length);
        assertEquals(1, integers[0].intValue());
        assertEquals(2, integers[1].intValue());
        assertEquals(Integer.class, integers.getClass().getComponentType());
    }
}
