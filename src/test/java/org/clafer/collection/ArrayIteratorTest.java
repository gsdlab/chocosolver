package org.clafer.collection;

import java.util.Iterator;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class ArrayIteratorTest {

    @Test
    public void testIterate() {
        Iterator<String> iter = new ArrayIterator(new String[]{"a", "b", "c", "d", "e"});
        assertTrue(iter.hasNext());
        assertEquals("a", iter.next());
        assertTrue(iter.hasNext());
        assertEquals("b", iter.next());
        assertTrue(iter.hasNext());
        assertEquals("c", iter.next());
        assertTrue(iter.hasNext());
        assertEquals("d", iter.next());
        assertTrue(iter.hasNext());
        assertEquals("e", iter.next());
        assertFalse(iter.hasNext());
    }

    @Test
    public void testIterateFromTo() {
        Iterator<String> iter = new ArrayIterator(new String[]{"a", "b", "c", "d", "e"}, 2, 4);
        assertEquals("c", iter.next());
        assertTrue(iter.hasNext());
        assertEquals("d", iter.next());
        assertFalse(iter.hasNext());
    }
}
