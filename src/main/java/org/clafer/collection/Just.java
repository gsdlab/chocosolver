package org.clafer.collection;

import org.clafer.common.Check;

/**
 * @param <T> the type of maybe
 * @author jimmy
 */
public class Just<T> extends Maybe<T> {

    private final T t;

    Just(T t) {
        this.t = Check.notNull(t);
    }

    @Override
    public boolean isNothing() {
        return false;
    }

    @Override
    public boolean isJust() {
        return true;
    }

    @Override
    public T fromMaybe(T t) {
        return this.t;
    }

    public T get() {
        return t;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Just<?>) {
            Just<?> other = (Just<?>) obj;
            return t.equals(other.t);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 934 ^ t.hashCode();
    }

    @Override
    public String toString() {
        return "Just " + t;
    }
}
