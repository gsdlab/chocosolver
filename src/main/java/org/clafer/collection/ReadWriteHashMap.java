package org.clafer.collection;

/**
 * @param <K> the type of keys
 * @param <V> the type of values
 * @author jimmy
 */
public class ReadWriteHashMap<K, V> extends ReadHashMap<K, V> {

    /**
     * Bind the key to the value.
     *
     * @param key the key
     * @param value the value
     * @throws IllegalStateException if the key already has a binding
     */
    public void put(K key, V value) throws IllegalStateException {
        if (map.put(key, value) != null) {
            throw new IllegalStateException(key + " already contained in the map.");
        }
    }
}
