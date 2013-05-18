package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import java.util.NoSuchElementException;

/**
 * An empty iterator.
 * 
 * @author jimmy
 */
public class EmptyIntIterator implements TIntIterator {

    private static final EmptyIntIterator iterator = new EmptyIntIterator();

    private EmptyIntIterator() {
    }

    /**
     * An iterator that is always empty.
     * 
     * @return the empty iterator singleton
     */
    public static EmptyIntIterator getIterator() {
        return iterator;
    }

    /** {@inheritDoc} */
    @Override
    public boolean hasNext() {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public int next() {
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
