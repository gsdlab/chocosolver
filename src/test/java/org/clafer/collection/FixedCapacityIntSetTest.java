package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.procedure.TIntProcedure;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class FixedCapacityIntSetTest {

    private static FixedCapacityIntSet set(int... values) {
        return new FixedCapacityIntSet(values);
    }

    // Reference implementation.
    private static TIntHashSet referenceSet(int... values) {
        return new TIntHashSet(values);
    }

    @Test
    public void testSize() {
        TIntSet set = new FixedCapacityIntSet(10);
        assertEquals(0, set.size());
        set.add(3);
        assertEquals(1, set.size());
        set.add(3);
        assertEquals(1, set.size());
        set.addAll(new int[]{3, 4, 6, 5});
        assertEquals(4, set.size());
    }

    @Test
    public void testIsEmpty() {
        TIntSet set = new FixedCapacityIntSet(1);
        assertTrue(set.isEmpty());
        set.add(1);
        assertFalse(set.isEmpty());
        set.remove(0);
        assertFalse(set.isEmpty());
        set.remove(1);
        assertTrue(set.isEmpty());
        set.add(0);
        assertFalse(set.isEmpty());
    }

    @Test
    public void testContains() {
        TIntSet set = new FixedCapacityIntSet(10);
        assertFalse(set.contains(3));
        set.add(3);
        assertTrue(set.contains(3));
        set.add(3);
        assertTrue(set.contains(3));
        set.addAll(new int[]{3, 4, 6, 5});
        assertTrue(set.contains(3));
        assertTrue(set.contains(4));
        assertTrue(set.contains(6));
        assertTrue(set.contains(5));
    }

    @Test
    public void testIterator() {
        TIntSet set = set(3, 4, 6, 5);
        TIntSet ans = referenceSet(3, 4, 6, 5);
        TIntIterator iter = set.iterator();

        assertTrue(iter.hasNext());
        int a = iter.next();
        iter.remove();
        ans.remove(a);
        assertEquals(ans, set);

        assertTrue(iter.hasNext());
        iter.next();

        assertTrue(iter.hasNext());
        int c = iter.next();
        iter.remove();
        ans.remove(c);
        assertEquals(ans, set);

        assertTrue(iter.hasNext());
        int d = iter.next();
        iter.remove();
        ans.remove(d);
        assertEquals(ans, set);

        assertFalse(iter.hasNext());
    }

    @Test
    public void testToArray() {
        TIntSet set = new FixedCapacityIntSet(5);
        set.add(3);
        set.add(4);
        set.add(6);
        set.add(5);
        {
            int[] array = set.toArray();
            Arrays.sort(array);
            assertArrayEquals(new int[]{3, 4, 5, 6}, array);
        }
        {
            int[] array = new int[10];
            set.toArray(array);
            Arrays.sort(array, 0, 4);
            assertEquals(3, array[0]);
            assertEquals(4, array[1]);
            assertEquals(5, array[2]);
            assertEquals(6, array[3]);
        }
    }

    @Test
    public void testAdd() {
        TIntSet set = new FixedCapacityIntSet(4);

        assertTrue(set.add(4));
        assertEquals(set(4), set);

        assertFalse(set.add(4));

        assertTrue(set.add(3));
        assertEquals(set(3, 4), set);

        assertTrue(set.add(6));
        assertEquals(set(3, 4, 6), set);

        assertFalse(set.add(3));

        assertTrue(set.add(5));
        assertEquals(set(3, 4, 6, 5), set);

        assertFalse(set.add(5));
        assertEquals(4, set.size());
    }

    @Test
    public void testRemove() {
        TIntSet set = set(3, 4, 6, 5);

        assertTrue(set.remove(4));
        assertEquals(set(3, 6, 5), set);

        assertFalse(set.remove(7));

        assertTrue(set.remove(6));
        assertEquals(set(3, 5), set);

        assertTrue(set.remove(5));
        assertEquals(set(3), set);

        assertFalse(set.remove(5));

        assertTrue(set.remove(3));
        assertEquals(set(), set);

        assertFalse(set.remove(3));
        assertFalse(set.remove(4));
        assertFalse(set.remove(6));
        assertFalse(set.remove(5));
        assertTrue(set.isEmpty());
    }

    @Test
    public void testContainsAll() {
        {
            TIntSet set = set(3, 4, 6, 5);
            assertFalse(set.containsAll(Arrays.asList(4, 5, 7)));
            assertTrue(set.containsAll(Arrays.asList(4, 5)));
        }
        {
            TIntSet set = set(3, 4, 6, 5);
            assertFalse(set.containsAll(new TIntArrayList(new int[]{4, 5, 7})));
            assertTrue(set.containsAll(new TIntArrayList(new int[]{4, 5})));
        }
        {
            TIntSet set = set(3, 4, 6, 5);
            assertFalse(set.containsAll(new int[]{4, 5, 7}));
            assertTrue(set.containsAll(new int[]{4, 5}));
        }
    }

    @Test
    public void testAddAll() {
        {
            TIntSet set = new FixedCapacityIntSet(5);
            set.addAll(Arrays.asList(3, 4, 6, 5));
            set.addAll(Arrays.asList(4, 5, 7));
            assertEquals(set(3, 4, 6, 5, 7), set);
        }
        {
            TIntSet set = new FixedCapacityIntSet(5);
            set.addAll(new TIntArrayList(new int[]{3, 4, 6, 5}));
            set.addAll(new TIntArrayList(new int[]{4, 5, 7}));
            assertEquals(set(3, 4, 6, 5, 7), set);
        }
        {
            TIntSet set = new FixedCapacityIntSet(5);
            set.addAll(new int[]{3, 4, 6, 5});
            set.addAll(new int[]{4, 5, 7});
            assertEquals(set(3, 4, 6, 5, 7), set);
        }
    }

    @Test
    public void testRetainAll() {
        {
            TIntSet set = set(3, 4, 6, 5);
            set.retainAll(Arrays.asList(4, 5, 7));
            assertEquals(set(4, 5), set);
        }
        {
            TIntSet set = set(3, 4, 6, 5);
            set.retainAll(new TIntArrayList(new int[]{4, 5, 7}));
            assertEquals(set(4, 5), set);
        }
        {
            TIntSet set = set(3, 4, 6, 5);
            set.retainAll(new int[]{4, 5, 7});
            assertEquals(set(4, 5), set);
        }
    }

    @Test
    public void testRemoveAll() {
        {
            TIntSet set = set(3, 4, 6, 5);
            set.removeAll(Arrays.asList(4, 5, 7));
            assertEquals(set(3, 6), set);
        }
        {
            TIntSet set = set(3, 4, 6, 5);
            set.removeAll(new TIntArrayList(new int[]{4, 5, 7}));
            assertEquals(set(3, 6), set);
        }
        {
            TIntSet set = set(3, 4, 6, 5);
            set.removeAll(new int[]{4, 5, 7});
            assertEquals(set(3, 6), set);
        }
    }

    @Test
    public void testClear() {
        TIntSet set = set(3, 4, 6, 5);
        set.clear();
        assertEquals(set(), set);
        set.add(3);
        set.add(1);
        assertEquals(set(1, 3), set);
    }

    @Test
    public void testForEach() {
        TIntSet set = set(3, 4, 6, 5);
        final TIntArrayList list = new TIntArrayList();
        assertFalse(set.forEach(new TIntProcedure() {
            @Override
            public boolean execute(int value) {
                list.add(value);
                return list.size() < 2;
            }
        }));
        assertEquals(2, list.size());

        list.clear();

        assertTrue(set.forEach(new TIntProcedure() {
            @Override
            public boolean execute(int value) {
                list.add(value);
                return list.size() < 5;
            }
        }));
        assertEquals(4, list.size());
    }

    @Test
    public void testEquals() {
        assertEquals(set(), set());
        assertNotEquals(set(), set(3));
        assertEquals(set(3, 4, 6, 5), set(6, 3, 4, 5));
        assertNotEquals(set(3, 4, 6, 5), set(3, 4, 6, 7));
    }

    @Test
    public void testToHashCode() {
        assertEquals(set().hashCode(), set().hashCode());
        assertEquals(set(3, 4, 6, 5).hashCode(), set(6, 3, 4, 5).hashCode());
    }

    @Test
    public void testToString() {
        assertEquals("{}", set().toString());
        assertEquals("{1}", set(1).toString());
        String string = set(1, 2).toString();
        assertTrue(string.equals("{1, 2}") || string.equals("{2, 1}"));
    }
}
