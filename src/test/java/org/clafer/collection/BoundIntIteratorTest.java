package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import java.util.Iterator;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class BoundIntIteratorTest {

    @Test
    public void testIterate() {
        TIntIterator iter = new BoundIntIterator(2, 4);
        assertTrue(iter.hasNext());
        assertEquals(2, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(3, iter.next());
        assertTrue(iter.hasNext());
        assertEquals(4, iter.next());
        assertFalse(iter.hasNext());
    }
}
