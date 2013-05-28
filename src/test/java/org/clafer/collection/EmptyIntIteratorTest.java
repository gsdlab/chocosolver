package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import static org.junit.Assert.assertFalse;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class EmptyIntIteratorTest {

    @Test
    public void testIterate() {
        TIntIterator iter = EmptyIntIterator.getIterator();
        assertFalse(iter.hasNext());
    }
}
