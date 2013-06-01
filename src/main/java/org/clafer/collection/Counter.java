package org.clafer.collection;

/**
 *
 * @author jimmy
 */
public class Counter {

    private int count = 0;

    public int next() {
        return count++;
    }
}
