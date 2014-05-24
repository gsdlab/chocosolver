package org.clafer.math;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class Variable {

    private final String name;
    private final int low, high;

    public Variable(String name, int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        this.name = Check.notNull(name);
        this.low = low;
        this.high = high;
    }

    public String getName() {
        return name;
    }

    public int getLowBound() {
        return low;
    }

    public int getHighBound() {
        return high;
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        return name;
    }
}
