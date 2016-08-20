package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import org.clafer.common.Util;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class ArrayIntIteratorTest {

    @Test
    public void testIterate() {
        TIntIterator iter = new ArrayIntIterator(Util.range(10, 15));
        assertTrue(iter.hasNext());
        assertEquals(10, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(11, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(12, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(13, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(14, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(15, iter.next());
        assertFalse(iter.hasNext());
    }

    @Test
    public void testIterateFromTo() {
        TIntIterator iter = new ArrayIntIterator(Util.range(10, 15), 2, 4);
        assertTrue(iter.hasNext());
        assertEquals(12, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(13, iter.next());
        assertFalse(iter.hasNext());
    }
}
