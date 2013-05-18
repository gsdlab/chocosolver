package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;

/**
 * In iterator over an interval.
 * 
 * @author jimmy
 */
public class BoundIntIterator implements TIntIterator {

    private int index;
    private final int high;

    /**
     * Iterate in order starting from low (inclusive) and ending in high (inclusive).
     * 
     * @param low the lowest value
     * @param high the highest value
     */
    public BoundIntIterator(int low, int high) {
        this.index = low;
        this.high = high;
    }

    /** {@inheritDoc} */
    @Override
    public boolean hasNext() {
        return index <= high;
    }

    /** {@inheritDoc} */
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
