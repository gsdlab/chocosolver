package org.clafer.collection;

/**
 * Types with an identity and an associative binary operation.
 *
 * @param <T> the type
 * @author jimmy
 */
public interface Monoid<T> {

    public T empty();

    public T append(T a, T b);

    public T concat(T... ts);
}
