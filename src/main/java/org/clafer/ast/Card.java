package org.clafer.ast;

/**
 * Immutable.
 * 
 * @author jimmy
 */
public class Card {

    private static final int UNBOUNDED_HIGH = Integer.MAX_VALUE;
    private final int low;
    private final int high;

    public Card() {
        this(0, UNBOUNDED_HIGH);
    }

    public Card(int low) {
        this(low, UNBOUNDED_HIGH);
    }

    public Card(int low, int high) {
        if (low < 0) {
            throw new IllegalArgumentException("low(" + low + ") < 0");
        }
        if (high == 0) {
            throw new IllegalArgumentException("high(" + high + ") = 0");
        }
        if (high < low) {
            throw new IllegalArgumentException("high(" + high + ") > low(" + low + ")");
        }
        this.low = low;
        this.high = high;
    }

    public boolean isExact() {
        return low == high;
    }

    public boolean isBounded() {
        return hasLow() || hasHigh();
    }

    public boolean hasLow() {
        return low != 0;
    }

    public int getLow() {
        return low;
    }

    public boolean hasHigh() {
        return high != UNBOUNDED_HIGH;
    }

    public int getHigh() {
        return high;
    }

    public Card add(Card addend) {
        if (hasHigh() && addend.hasHigh()) {
            return new Card(low + addend.low, high + addend.high);
        }
        return new Card(low + addend.low);
    }

    public Card mult(Card factor) {
        if (hasHigh() && factor.hasHigh()) {
            return new Card(low * factor.low, high * factor.high);
        }
        return new Card(low * factor.low);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Card) {
            Card other = (Card) obj;
            return low == other.low && high == other.high;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return low ^ high;
    }

    @Override
    public String toString() {
        if (!hasHigh()) {
            return hasLow() ? low + "..*" : "*";
        }
        return isExact() ? Integer.toString(low) : low + ".." + high;
    }
}
