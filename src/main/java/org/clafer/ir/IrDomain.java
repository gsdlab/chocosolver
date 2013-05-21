package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;

/**
 *
 * @author jimmy
 */
public interface IrDomain {

    public boolean isBounded();

    public boolean contains(int value);

    public int getLowerBound();

    public int getUpperBound();

    public boolean isEmpty();

    public int size();

    public int[] getValues();

    public TIntIterator iterator();

    public TIntIterator iterator(boolean increasing);
}
