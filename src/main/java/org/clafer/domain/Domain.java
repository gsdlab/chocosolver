package org.clafer.domain;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;

/**
 * Immutable domain over integers.
 *
 * @author jimmy
 */
public interface Domain {

    /**
     * Checks if this domain is defined as a lower and upper bound. If the
     * domain is bounded then it contains every value between its lower and
     * upper bound.
     *
     * @return {@code true} if and only if this domain is a contiguous interval,
     * {@code false} otherwise
     */
    public boolean isBounded();

    /**
     * Checks if a value is within this domain.
     *
     * @param value test this value
     * @return {@code true} if and only if this domain contains the
     * {@code value}, {@code false} otherwise
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
     * Check if this domain is a (non-strict) subset of another domain.
     *
     * @param superset
     * @return {@code true} if and only if this domain is a subset of
     * {@code superset}, {@code false} otherwise
     */
    public boolean isSubsetOf(Domain superset);

    /**
     * Check if this domain intersects another domain.
     *
     * @param other
     * @return {@code true} if and only if this domain is intersects
     * {@code other}, {@code false} otherwise
     */
    public boolean intersects(Domain other);

    /**
     * Add an element into this domain.
     *
     * @param value
     * @return {@code this.union({value})}
     */
    public Domain insert(int value);

    /**
     * Remove an element from this domain.
     *
     * @param value
     * @return {@code this.difference({value})}
     */
    public Domain remove(int value);

    /**
     * Remove all elements less than the bound from this domain.
     *
     * @param low
     * @return {@code this.intersection({low, low + 1, ...})
     */
    public Domain boundLow(int low);

    /**
     * Remove all elements greater than the bound from this domain.
     *
     * @param high
     * @return {@code this.intersection({high, high - 1, ...})
     */
    public Domain boundHigh(int high);

    /**
     * Remove all elements less than the low bound or greater than the high
     * bound from this domain.
     *
     * @param low
     * @param high
     * @return {@code this.intersection({low, low + 1, ..., high})
     */
    public Domain boundBetween(int low, int high);

    public Domain minus();

    /**
     * Subtract this domain with the other domain.
     *
     * @param other
     * @return the difference of this domain with the other domain
     */
    public Domain difference(Domain other);

    /**
     * Intersect this domain with the other domain.
     *
     * @param other
     * @return the intersection of this domain with the other domain
     */
    public Domain intersection(Domain other);

    /**
     * Union this domain with the other domain.
     *
     * @param other
     * @return the union of this domain with the other domain
     */
    public Domain union(Domain other);

    /**
     * Shift the elements in this domain.
     *
     * @param c
     * @return the shifted domain
     */
    public Domain offset(int c);

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
     * Iterate over the domain in the specified order.
     *
     * @param increasing increasing or decreasing order
     * @return an iterator over the values in this domain in the order specified
     */
    public TIntIterator iterator(boolean increasing);

    /**
     * Put the contents of this domain inside the collection.
     *
     * @param collection the collection
     */
    public void transferTo(TIntCollection collection);
}
