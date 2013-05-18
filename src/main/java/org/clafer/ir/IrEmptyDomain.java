package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import org.clafer.collection.EmptyIntIterator;

/**
 *
 * @author jimmy
 */
public class IrEmptyDomain extends IrDomain {

    @Override
    public boolean isBounded() {
        return false;
    }

    @Override
    public boolean contains(int value) {
        return false;
    }

    @Override
    public int getLowerBound() {
        throw new IrException();
    }

    @Override
    public int getUpperBound() {
        throw new IrException();
    }

    @Override
    public boolean isEmpty() {
        return true;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public int[] getValues() {
        return new int[]{};
    }

    @Override
    public TIntIterator iterator() {
        return EmptyIntIterator.getIterator();
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof IrEmptyDomain;
    }

    @Override
    public int hashCode() {
        return 305419896;
    }

    @Override
    public String toString() {
        return "{}";
    }
    
}
