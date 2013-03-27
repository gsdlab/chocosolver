package org.clafer.collection;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author jimmy
 */
public class ReadHashMap<K, V> implements ReadMap<K, V> {

    protected final Map<K, V> map;

    public ReadHashMap() {
        this.map = new HashMap<K, V>();
    }

    public ReadHashMap(Map<K, V> map) {
        this.map = new HashMap<K, V>(map);
    }

    @Override
    public boolean constainsKey(K key) {
        return map.containsKey(key);
    }

    @Override
    public boolean constainsValue(V value) {
        return map.containsValue(value);
    }

    @Override
    public V get(K key) {
        return map.get(key);
    }

    @Override
    public Collection<K> getKeys() {
        return map.keySet();
    }

    @Override
    public Collection<V> getValues() {
        return map.values();
    }

    @Override
    public int size() {
        return map.size();
    }

    @Override
    public ReadHashMap readOnly() {
        return new ReadHashMap(map);
    }
}
