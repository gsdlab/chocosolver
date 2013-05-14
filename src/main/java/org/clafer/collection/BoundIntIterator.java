package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;

/**
 *
 * @author jimmy
 */
public class BoundIntIterator implements TIntIterator {

    private int index;
    private final int high;

    /**
     * @param low - Inclusive
     * @param high - Inclusive
     */
    public BoundIntIterator(int low, int high) {
        this.index = low;
        this.high = high;
    }

    @Override
    public boolean hasNext() {
        return index <= high;
    }

    @Override
    public int next() {
        return index++;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
