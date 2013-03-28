package org.clafer.collection;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class Appender<T> {

    private final List<T> items = new ArrayList<T>();
    private final Class<T> type;

    private Appender(Class<T> type) {
        this.type = type;
    }

    public static <T> Appender<T> build() {
        return new Appender<T>(null);
    }

    public static <T> Appender<T> build(Class<T> type) {
        return new Appender<T>(type);
    }

    public Appender<T> add(T item) {
        this.items.add(Check.notNull(item));
        return this;
    }

    public Appender<T> addAll(T[] items) {
        return addAll(Arrays.asList(items));
    }

    public Appender<T> addAll(Collection<? extends T> items) {
        this.items.addAll(Check.noNulls(items));
        return this;
    }

    public T[] toArray() {
        if (items.isEmpty() && type == null) {
            throw new IllegalStateException();
        }
        Class<?> componentType = type == null ? items.get(0).getClass() : type;
        return items.toArray((T[]) Array.newInstance(componentType, items.size()));
    }
}
