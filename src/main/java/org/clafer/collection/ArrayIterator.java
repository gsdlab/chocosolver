package org.clafer.collection;

import java.util.Iterator;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 * @param <T> the type of elements returned by this iterator
 */
public class ArrayIterator<T> implements Iterator<T> {

    private final T[] array;
    private int index;
    private final int to;

    public ArrayIterator(T[] array) {
        this(array, 0, array.length);
    }

    /**
     * Iterate an array in order starting in position from (inclusive) and
     * ending in position to (exclusive).
     *
     * @param array iterate this array
     * @param from start iterating from this index
     * @param to stop before this index
     */
    public ArrayIterator(T[] array, int from, int to) {
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
        this.index = from;
        this.to = to;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasNext() {
        return index < to;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T next() {
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
