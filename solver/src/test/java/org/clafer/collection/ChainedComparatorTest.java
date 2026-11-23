package org.clafer.collection;

import java.util.Comparator;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class ChainedComparatorTest {

    @Test
    public void testCompare() {
        Comparator<String> comparator = new ChainedComparator<>(
                Comparator.comparing(String::length), Comparator.naturalOrder());
        assertEquals(0, comparator.compare("abc", "abc"));
        assertTrue(comparator.compare("abc", "def") < 0);
        assertTrue(comparator.compare("def", "abc") > 0);
        assertTrue(comparator.compare("abc", "defg") < 0);
        assertTrue(comparator.compare("abcd", "efg") > 0);
    }

    @Test
    public void testCompareSingleChain() {
        Comparator<String> comparator = new ChainedComparator<>(Comparator.comparing(String::length));
        assertEquals(0, comparator.compare("abc", "def"));
        assertTrue(comparator.compare("abc", "defg") < 0);
        assertTrue(comparator.compare("abcd", "efg") > 0);
    }
}
