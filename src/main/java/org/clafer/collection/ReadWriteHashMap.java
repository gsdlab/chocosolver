package org.clafer.collection;

/**
 * @param <K> the type of keys
 * @param <V> the type of values
 * @author jimmy
 */
public class ReadWriteHashMap<K, V> extends ReadHashMap<K, V> {

    public void put(K key, V value) {
        if (map.put(key, value) != null) {
            throw new IllegalStateException(key + " already contained in the map.");
        }
    }
}
