package org.clafer.ast;

import java.io.Serializable;

/**
 * Low and high cardinality. Immutable.
 *
 * @author jimmy
 */
public class Card implements Serializable {

    private static final int UNBOUNDED_HIGH = Integer.MAX_VALUE;
    private final int low;
    private final int high;

    /**
     * Construct an unbounded cardinality.
     */
    public Card() {
        this(0, UNBOUNDED_HIGH);
    }

    /**
     * Construct a cardinality bounded from below, unbounded above.
     *
     * @param low the low cardinality
     */
    public Card(int low) {
        this(low, UNBOUNDED_HIGH);
    }

    /**
     * Construct a cardinality bounded from below and above.
     *
     * @param low the low cardinality
     * @param high the high cardinality
     */
    public Card(int low, int high) {
        if (low < 0) {
            throw new IllegalArgumentException("low(" + low + ") < 0");
        }
        if (high < low) {
            throw new IllegalArgumentException("high(" + high + ") > low(" + low + ")");
        }
        this.low = low;
        this.high = high;
    }

    /**
     * Detects if this cardinality is so restrictive that only one possible
     * value is allowed.
     *
     * @return {@code true} if and only if low and high cardinality is the same,
     * {@code false} otherwise
     */
    public boolean isExact() {
        return low == high;
    }

    /**
     * Detects if this cardinality is either bounded below and/or above.
     *
     * @return {@code true} if and only if this cardinality is not unbounded,
     * {@code false} otherwise
     */
    public boolean isBounded() {
        return hasLow() || hasHigh();
    }

    /**
     * Detects if this cardinality is bounded below.
     *
     * @return {@code true} if and only if this cardinality is bounded below,
     * {@code false} otherwise
     */
    public boolean hasLow() {
        return low != 0;
    }

    /**
     * Returns the low cardinality or 0 if unbounded from below.
     *
     * @return the low cardinality
     */
    public int getLow() {
        return low;
    }

    /**
     * Detects if this cardinality is bounded above.
     *
     * @return {@code true} if and only if this cardinality is bounded above,
     * {@code false} otherwise
     */
    public boolean hasHigh() {
        return high != UNBOUNDED_HIGH;
    }

    /**
     * Returns the high cardinality or Integer.MAX_VALUE if unbounded form
     * above.
     *
     * @return the high cardinality
     */
    public int getHigh() {
        return high;
    }

    /**
     * Add two cardinalities together.
     *
     * @param addend the other cardinality
     * @return the sum of this and the other cardinality
     */
    public Card add(Card addend) {
        if (hasHigh() && addend.hasHigh()) {
            return new Card(low + addend.low, high + addend.high);
        }
        return new Card(low + addend.low);
    }

    /**
     * Multiply two cardinalities together.
     *
     * @param factor the other cardinality
     * @return the product of this and the other cardinality
     */
    public Card mult(Card factor) {
        if (hasHigh() && factor.hasHigh()) {
            return new Card(low * factor.low, high * factor.high);
        }
        return new Card(low * factor.low);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Card) {
            Card other = (Card) obj;
            return low == other.low && high == other.high;
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return low ^ high;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        if (!hasHigh()) {
            return hasLow() ? low + "..*" : "*";
        }
        return isExact() ? Integer.toString(low) : low + ".." + high;
    }
}
