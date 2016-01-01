package org.clafer.ontology;

import gnu.trove.map.TIntObjectMap;
import gnu.trove.map.TObjectIntMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import gnu.trove.map.hash.TObjectIntHashMap;
import java.util.Collection;
import java.util.Set;

/**
 *
 * @author jimmy
 */
public class IdMap<T> {

    private final TObjectIntMap<T> idMap = new TObjectIntHashMap<>();
    private final TIntObjectMap<T> keyMap = new TIntObjectHashMap<>();

    public int getId(T key) {
        int newId = idMap.size();
        int id = idMap.adjustOrPutValue(key, 0, newId);
        if (id == newId) {
            keyMap.put(id, key);
        }
        return id;
    }

    public int[] getIds(Collection<T> keys) {
        int[] ids = new int[keys.size()];
        int i = 0;
        for (T key : keys) {
            ids[i++] = getId(key);
        }
        return ids;
    }

    public T getKey(int id) {
        return keyMap.get(id);
    }

    public Set<T> keySet() {
        return idMap.keySet();
    }
}
