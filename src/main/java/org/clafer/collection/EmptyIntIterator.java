package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;
import java.util.NoSuchElementException;

/**
 *
 * @author jimmy
 */
public class EmptyIntIterator implements TIntIterator {

    private static final EmptyIntIterator iterator = new EmptyIntIterator();

    private EmptyIntIterator() {
    }

    public static EmptyIntIterator getIterator() {
        return iterator;
    }

    @Override
    public boolean hasNext() {
        return false;
    }

    @Override
    public int next() {
        throw new NoSuchElementException();
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
