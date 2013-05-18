package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import org.clafer.common.Check;

/**
 * In iterator for an array of integers.
 * 
 * @author jimmy
 */
public class ArrayIntIterator implements TIntIterator {

    private final int[] array;
    private int index;
    private final int to;

    /**
     * Iterate an array in order from the first to last element of the array.
     * @param array 
     */
    public ArrayIntIterator(int[] array) {
        this(array, 0, array.length);
    }

    /**
     * Iterate an array in order starting in position from (inclusive) and ending
     * in position to (exclusive).
     * 
     * @param array iterate this array
     * @param from start iterating from this index
     * @param to stop before this index
     */
    public ArrayIntIterator(int[] array, int from, int to) {
        this.array = Check.notNull(array);
        this.index = from;
        this.to = to;

        if (to < from) {
            throw new IllegalArgumentException();
        }
        if (from < 0) {
            throw new IllegalArgumentException();
        }
        if (to > array.length) {
            throw new IllegalArgumentException();
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean hasNext() {
        return index < to;
    }

    /** {@inheritDoc} */
    @Override
    public int next() {
        return array[index++];
    }

    /**
     * Not supported.
     * 
     * @throws UnsupportedOperationException if invoked
     */
    @Override
    public void remove() throws UnsupportedOperationException {
        throw new UnsupportedOperationException();
    }
}
