package org.clafer.tree;

/**
 *
 * @author jimmy
 */
public class Card {

    private static final int UNBOUNDED_HIGH = Integer.MAX_VALUE;
    private final int low;
    private final int high;

    public Card() {
        this(0);
    }

    public Card(int low) {
        this(low, UNBOUNDED_HIGH);
    }

    public Card(int low, int high) {
        if (low < 0) {
            throw new IllegalArgumentException();
        }
        if (high < low) {
            throw new IllegalArgumentException();
        }
        this.low = low;
        this.high = high;
    }

    public boolean isExact() {
        return getLow() == getHigh();
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
            return new Card(low * factor.low, high * high);
        }
        return new Card(low * factor.low);
    }

    @Override
    public String toString() {
        if (!hasHigh()) {
            return hasLow() ? low + "..*" : "*";
        }
        return isExact() ? Integer.toString(low) : low + ".." + high;
    }
}
