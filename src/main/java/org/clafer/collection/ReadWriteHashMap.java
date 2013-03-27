package org.clafer.collection;

/**
 *
 * @author jimmy
 */
public class ReadWriteHashMap<K, V> extends ReadHashMap<K, V> {

    public void put(K key, V value) {
        if (map.put(key, value) != null) {
            throw new IllegalStateException();
        }
    }
}
