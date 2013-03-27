package org.clafer.collection;

import java.util.Collection;

/**
 *
 * @author jimmy
 */
public interface ReadMap<K, V> {

    public boolean constainsKey(K key);

    public boolean constainsValue(V value);

    public V get(K key);

    public Collection<K> getKeys();

    public Collection<V> getValues();

    public int size();

    public ReadMap readOnly();
}
