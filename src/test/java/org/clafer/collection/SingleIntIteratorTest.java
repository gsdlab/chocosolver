package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SingleIntIteratorTest {

    @Test
    public void testIterate() {
        TIntIterator iter = new SingleIntIterator(42);
        assertTrue(iter.hasNext());
        assertEquals(42, iter.next());
        assertFalse(iter.hasNext());
    }
}
