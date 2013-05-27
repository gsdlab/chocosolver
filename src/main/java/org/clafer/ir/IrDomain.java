package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;

/**
 * Integer domain.
 *
 * @author jimmy
 */
public interface IrDomain {

    /**
     * Checks if this domain is a contiguous interval. This domain is contiguous
     * if it contains every value between its lower and upper bound.
     *
     * @return {@code true} if and only if this domain is a contiguous interval,
     * {@code false} otherwise
     */
    public boolean isBounded();

    /**
     * Checks if a value is within this domain.
     *
     * @param value test this value
     * @return {@code true} if and only if this domain contains the {@code value},
     *         {@code false} otherwise
     */
    public boolean contains(int value);

    /**
     * Returns the smallest integer contained in this domain. Undefined if this
     * domain is empty.
     *
     * @return the smallest integer contained in this domain
     */
    public int getLowBound();

    /**
     * Returns the largest integer contained in this domain. Undefined if this
     * domain is empty.
     *
     * @return the largest integer contained in this domain
     */
    public int getHighBound();

    /**
     * Checks if this domain contains any values.
     *
     * @return {@code true} if and only if the size of this domain is zero,
     * {@code false} otherwise
     */
    public boolean isEmpty();

    /**
     * Returns how many values are contained in this domain.
     * 
     * @return the size of this domain
     */
    public int size();

    /**
     * Returns all the values contained in this domain.
     * 
     * @return values contained in this domain
     */
    public int[] getValues();

    /**
     * Iterate over the domain in increasing order.
     * 
     * @return an iterator over the values in this domain in increasing order
     */
    public TIntIterator iterator();

    /**
     * * Iterate over the domain in the specified order.
     * 
     * @param increasing increasing or decreasing order
     * @return an iterator over the values in this domain in the order specified
     */
    public TIntIterator iterator(boolean increasing);
}
