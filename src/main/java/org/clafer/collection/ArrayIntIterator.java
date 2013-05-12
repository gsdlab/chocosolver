package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class ArrayIntIterator implements TIntIterator {

    private final int[] array;
    private int index;
    private final int to;

    public ArrayIntIterator(int[] array) {
        this(array, 0, array.length);
    }

    /**
     * @param array
     * @param from - Inclusive
     * @param to - Exclusive
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

    @Override
    public boolean hasNext() {
        return index < to;
    }

    @Override
    public int next() {
        return array[index++];
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
