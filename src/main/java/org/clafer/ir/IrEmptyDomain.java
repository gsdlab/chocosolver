package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import org.clafer.collection.EmptyIntIterator;

/**
 * A domain of size 0. Contains nothing.
 *
 * @author jimmy
 */
public class IrEmptyDomain implements IrDomain {

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBounded() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(int value) {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLowBound() {
        throw new IrException("Emtpy domain does not have a low bound.");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getHighBound() {
        throw new IrException("Emtpy domain does not have a high bound.");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty() {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] getValues() {
        return new int[]{};
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TIntIterator iterator() {
        return EmptyIntIterator.getIterator();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TIntIterator iterator(boolean increasing) {
        return EmptyIntIterator.getIterator();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        return obj instanceof IrEmptyDomain;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 305419896;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return "{}";
    }
}
