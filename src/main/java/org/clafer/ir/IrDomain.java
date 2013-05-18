package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import java.util.Arrays;
import org.clafer.collection.ArrayIntIterator;
import org.clafer.collection.BoundIntIterator;
import org.clafer.collection.EmptyIntIterator;

/**
 *
 * @author jimmy
 */
public abstract class IrDomain {

    public abstract boolean isBounded();

    public abstract boolean contains(int value);

    public abstract int getLowerBound();

    public abstract int getUpperBound();

    public abstract boolean isEmpty();

    public abstract int size();

    public abstract int[] getValues();

    public abstract TIntIterator iterator();

    @Override
    public abstract boolean equals(Object obj);

    @Override
    public abstract int hashCode();
}
