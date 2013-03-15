package org.clafer.func;

/**
 *
 * @author jimmy
 */
public class LinearFunc implements Func<Integer, Integer> {

    private final int m, b;

    public LinearFunc(int m, int b) {
        this.m = m;
        this.b = b;
    }

    @Override
    public Integer apply(Integer arg) {
        return m * arg + b;
    }
}
