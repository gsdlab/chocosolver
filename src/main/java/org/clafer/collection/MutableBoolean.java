package org.clafer.collection;

/**
 *
 * @author jimmy
 */
public class MutableBoolean {

    private boolean value;

    public boolean getValue() {
        return value;
    }

    public boolean isSet() {
        return value;
    }

    public boolean isClear() {
        return !value;
    }

    public void setValue(boolean value) {
        this.value = value;
    }

    public void set() {
        setValue(true);
    }

    public void clear() {
        setValue(false);
    }
}
