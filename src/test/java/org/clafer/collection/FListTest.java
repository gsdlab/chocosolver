package org.clafer.collection;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import static org.clafer.collection.FList.*;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class FListTest {

    private static List<Integer> list(Integer... values) {
        return Arrays.asList(values);
    }

    @Test
    public void testEmpty() {
        assertTrue(isEmpty(empty()));
        assertFalse(isEmpty(single(1)));
    }

    @Test
    public void testSingle() {
        FList<Integer> list = single(1);
        assertEquals(1, list.getHead().intValue());
        assertTrue(isEmpty(list.getTail()));
    }

    @Test
    public void testCons() {
        FList<Integer> tail = single(1);
        FList<Integer> list = cons(2, tail);
        assertEquals(2, list.getHead().intValue());
        assertEquals(tail, list.getTail());
    }

    @Test
    public void testSnoc() {
        FList<Integer> head = single(1);
        FList<Integer> list = snoc(head, 2);
        assertEquals(1, list.getHead().intValue());
        assertEquals(single(2), list.getTail());
    }

    @Test
    public void testToList() {
        assertEquals(list(1), single(1).toList());
        assertEquals(list(1, 2, 3), cons(1, snoc(single(2), 3)).toList());
    }

    @Test
    public void testIterator() {
        Iterator<Integer> iter = cons(1, snoc(single(2), 3)).iterator();

        assertTrue(iter.hasNext());
        assertEquals(1, iter.next().intValue());
        assertTrue(iter.hasNext());
        assertEquals(2, iter.next().intValue());
        assertTrue(iter.hasNext());
        assertEquals(3, iter.next().intValue());
        assertFalse(iter.hasNext());
    }

    @Test
    public void testEquals() {
        assertTrue(Util.equals(empty(), empty()));
        assertFalse(Util.equals(FList.<Integer>empty(), single(1)));
        assertTrue(Util.equals(single(1), single(1)));
        assertFalse(Util.equals(single(2), single(1)));
        assertFalse(Util.equals(cons(1, single(2)), single(1)));
        assertTrue(Util.equals(cons(1, single(2)), cons(1, single(2))));
        assertFalse(Util.equals(cons(1, single(2)), cons(2, single(1))));
        assertFalse(Util.equals(cons(1, single(2)), FList.<Integer>empty()));
    }

    @Test
    public void testToHashCode() {
        assertEquals(cons(1, cons(2, single(3))).hashCode(), cons(1, snoc(single(2), 3)).hashCode());
    }

    @Test
    public void testToString() {
        assertEquals("[1, 2, 3]", cons(1, snoc(single(2), 3)).toString());
    }
}
