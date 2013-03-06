package org.clafer.collection;

import java.util.NoSuchElementException;

/**
 *
 * @author jimmy
 */
public class IntArrayIterator implements IntIterator {

    private final int[] array;
    private int index;
    private final int to;

    public IntArrayIterator(int[] array, int from, int to) {
        this.array = array;
        this.index = from;
        this.to = to;
    }

    @Override
    public boolean hasNext() {
        return index < to;
    }

    @Override
    public int next() {
        if (index < to) {
            return array[index++];
        }
        throw new NoSuchElementException();
    }
}
