package org.clafer.tree;

/**
 *
 * @author jimmy
 */
public class Card {

    private final int low;
    private final Integer high;

    public Card() {
        this(0);
    }

    public Card(int low) {
        if (low < 0) {
            throw new IllegalArgumentException();
        }
        this.low = low;
        this.high = null;
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
        return hasHigh() && getLow() == getHigh();
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
        return high != null;
    }

    public int getHigh() {
        return hasHigh() ? high.intValue() : Integer.MAX_VALUE;
    }

    @Override
    public String toString() {
        if (hasHigh()) {
            return hasLow() ? low + "..*" : "*";
        }
        return isExact() ? Integer.toString(low) : low + ".." + getHigh();
    }
}
