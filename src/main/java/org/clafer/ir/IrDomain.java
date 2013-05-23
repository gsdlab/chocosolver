package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;

/**
 * Integer domain.
 *
 * @author jimmy
 */
public interface IrDomain {

    /**
     * Checks if the domain is a contiguous interval. The the domain is
     * contiguous if it contains every value between its lower and upper bound.
     *
     * @return {@code true} if and only if the domain is a contiguous interval,
     * {@code false} otherwise
     */
    public boolean isBounded();

    /**
     * @param value test this value
     * @return {@code true} if and only if the domain contains the {@code value},
     *         {@code false} otherwise
     */
    public boolean contains(int value);

    /**
     * @return the smallest integer contained in the domain
     */
    public int getLowBound();

    /**
     * @return the largest integer contained in the domain
     */
    public int getHighBound();

    /**
     * @return {@code true} if and only if the size of the domain is zero,
     * {@code false} otherwise
     */
    public boolean isEmpty();

    /**
     * @return the size of the domain
     */
    public int size();

    /**
     * @return all values contained in the domain
     */
    public int[] getValues();

    /**
     * @return an iterator over the values in the domain in increasing order
     */
    public TIntIterator iterator();

    /**
     * @param increasing increasing or decreasing order
     * @return an iterator over the values in the domain in the order specified
     */
    public TIntIterator iterator(boolean increasing);
}
