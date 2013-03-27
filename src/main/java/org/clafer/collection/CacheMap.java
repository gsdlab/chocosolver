package org.clafer.collection;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class CacheMap<K, V> extends ReadHashMap<K, V> {

    @Override
    public V get(K key) {
        V value = map.get(key);
        if (value == null) {
            value = Check.notNull(cache(key));
            map.put(key, value);
        }
        return value;
    }

    protected abstract V cache(K key);
}
