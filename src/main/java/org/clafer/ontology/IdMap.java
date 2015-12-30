package org.clafer.ontology;

import gnu.trove.map.TObjectIntMap;
import gnu.trove.map.hash.TObjectIntHashMap;
import java.util.Set;

/**
 *
 * @author jimmy
 */
public class IdMap<T> {

    private final TObjectIntMap<T> idMap = new TObjectIntHashMap<>();

    public int getId(T t) {
        return idMap.adjustOrPutValue(t, 0, idMap.size());
    }

    public Set<T> keySet() {
        return idMap.keySet();
    }
}
