package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;

/**
 * An iterator over an interval in increasing order.
 *
 * @author jimmy
 */
public class BoundIntIterator implements TIntIterator {

    private int index;
    private final int high;

    /**
     * Iterate in increasing order starting from low (inclusive) and ending in
     * high (inclusive).
     *
     * @param low the lowest value
     * @param high the highest value
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

    /**
     * Not supported.
     *
     * @throws UnsupportedOperationException if invoked
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
