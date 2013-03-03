package org.clafer.func;

/**
 *
 * @author jimmy
 */
public class ConstFunc implements IntFunc {

    private final int value;

    public ConstFunc(int value) {
        this.value = value;
    }

    @Override
    public int apply(int arg) {
        return value;
    }
}
