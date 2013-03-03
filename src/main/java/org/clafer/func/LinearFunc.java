package org.clafer.func;

/**
 *
 * @author jimmy
 */
public class LinearFunc implements IntFunc {

    private final int m, b;

    public LinearFunc(int m, int b) {
        this.m = m;
        this.b = b;
    }

    @Override
    public int apply(int arg) {
        return m * arg + b;
    }
}
