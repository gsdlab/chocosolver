package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import org.clafer.common.Check;

/**
 * In iterator for an array of integers in order of decreasing index.
 *
 * @author jimmy
 */
public class ReverseArrayIntIterator implements TIntIterator {

    private final int[] array;
    private int index;
    private final int from;

    /**
     * Iterate an array in reverse order from the last to first element of the
     * array.
     *
     * @param array
     */
    public ReverseArrayIntIterator(int[] array) {
        this(array, 0, array.length);
    }

    /**
     * Iterate an array in reverse order starting in position to (exclusive) and
     * ending in position from (inclusive).
     *
     * @param array iterate this array
     * @param from stop iterating at this index
     * @param to start after this index
     */
    public ReverseArrayIntIterator(int[] array, int from, int to) {
        if (to < from) {
            throw new IllegalArgumentException();
        }
        if (from < 0) {
            throw new IllegalArgumentException();
        }
        if (to > array.length) {
            throw new IllegalArgumentException();
        }
        this.array = Check.notNull(array);
        this.index = to;
        this.from = from;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasNext() {
        return index > from;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int next() {
        return array[--index];
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
