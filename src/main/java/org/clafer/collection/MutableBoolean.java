package org.clafer.collection;

/**
 *
 * @author jimmy
 */
public class MutableBoolean {

    private boolean set;

    public MutableBoolean(boolean set) {
        this.set = set;
    }

    public MutableBoolean() {
        this(false);
    }

    public void set() {
        set = true;
    }

    public void clear() {
        set = false;
    }

    public boolean isSet() {
        return set;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof MutableBoolean) {
            MutableBoolean other = (MutableBoolean) obj;
            return set == other.set;
        }
        return false;
    }

    @Override
    public int hashCode() {
        // Same values as java.lang.Boolean.hashcode
        return set ? 1231 : 1237;
    }

    @Override
    public String toString() {
        return Boolean.toString(set);
    }
}
