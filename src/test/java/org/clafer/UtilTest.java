package org.clafer;

import java.util.Arrays;
import java.util.List;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class UtilTest {

    private static List<Integer> list(Integer... items) {
        return Arrays.asList(items);
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
