package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import java.util.NoSuchElementException;

/**
 * Iterate over a single value.
 * 
 * @author jimmy
 */
public class SingleIntIterator implements TIntIterator {

    private final int value;
    private boolean hasNext = true;

    /**
     * Iterate over a single value.
     * 
     * @param value the single value
     */
    public SingleIntIterator(int value) {
        this.value = value;
    }

    /** {@inheritDoc} */
    @Override
    public boolean hasNext() {
        return hasNext;
    }

    /** {@inheritDoc} */
    @Override
    public int next() {
        if (hasNext) {
            hasNext = false;
            return value;
        }
        throw new NoSuchElementException();
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
