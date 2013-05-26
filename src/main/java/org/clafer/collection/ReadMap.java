package org.clafer.collection;

import java.util.Collection;

/**
 * @param <K> the type of keys
 * @param <V> the type of values
 * @author jimmy
 */
public interface ReadMap<K, V> {

    public boolean constainsKey(K key);

    public boolean constainsValue(V value);

    public V get(K key);

    public Collection<K> getKeys();

    public Collection<V> getValues();

    public int size();

    public ReadMap<K, V> readOnly();
}
