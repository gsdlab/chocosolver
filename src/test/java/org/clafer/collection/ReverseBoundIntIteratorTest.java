package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class ReverseBoundIntIteratorTest {

    @Test
    public void testIterate() {
        TIntIterator iter = new ReverseBoundIntIterator(2, 4);
        assertTrue(iter.hasNext());
        assertEquals(4, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(3, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(2, iter.next());
        assertFalse(iter.hasNext());
    }
}
