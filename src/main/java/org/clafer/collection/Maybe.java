package org.clafer.collection;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

/**
 * A option type. Does not allow nulls.
 *
 * @param <T> the type of maybe
 * @author jimmy
 */
public abstract class Maybe<T> {

    public abstract boolean isNothing();

    public abstract boolean isJust();

    public abstract T fromMaybe(T t);

    public static <T> Nothing<T> nothing() {
        @SuppressWarnings("unchecked")
        Nothing<T> maybe = (Nothing<T>) Nothing.Singleton;
        return maybe;
    }

    public static <T> Just<T> just(T t) {
        return new Just<>(t);
    }

    public static <T> List<T> filterJust(List<Maybe<T>> maybes) {
        List<T> justs = new ArrayList<>();
        for (Maybe<T> maybe : maybes) {
            T t = maybe.fromMaybe(null);
            if (t != null) {
                justs.add(t);
            }
        }
        return justs;
    }

    @SafeVarargs
    public static <T> T[] filterJust(Maybe<T>[] maybes, T... array) {
        List<T> justs = new ArrayList<>();
        for (Maybe<T> maybe : maybes) {
            T t = maybe.fromMaybe(null);
            if (t != null) {
                justs.add(t);
            }
        }
        @SuppressWarnings("unchecked")
        T[] to = array.length == justs.size()
                ? array
                : (T[]) Array.newInstance(array.getClass().getComponentType(), justs.size());
        return justs.toArray(to);
    }
}
