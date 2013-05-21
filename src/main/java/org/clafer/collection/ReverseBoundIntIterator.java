package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;

/**
 * An iterator over an interval in decreasing order.
 * 
 * @author jimmy
 */
public class ReverseBoundIntIterator implements TIntIterator {

    private int index;
    private final int low;

    /**
     * Iterate in decreasing order starting from high (inclusive) and ending in
     * low (inclusive).
     *
     * @param low the lowest value
     * @param high the highest value
     */
    public ReverseBoundIntIterator(int low, int high) {
        this.index = high;
        this.low = low;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean hasNext() {
        return index >= low;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int next() {
        return index--;
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