package org.clafer.collection;

import gnu.trove.iterator.TObjectIntIterator;
import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.map.hash.TIntObjectHashMap;
import gnu.trove.map.hash.TObjectIntHashMap;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @param <V> the type of the data
 * @author jimmy
 */
public class DisjointSets<V> {

    private final TObjectIntHashMap<V> nodes = new TObjectIntHashMap<V>();
    private final TIntIntHashMap parents = new TIntIntHashMap();
    private final TIntIntHashMap ranks = new TIntIntHashMap();

    private int getNode(V i) {
        int n = nodes.get(i);
        if (n == 0) {
            n = nodes.size() + 1;
            nodes.put(i, n);
            parents.put(n, n);
        }
        return n;
    }

    private int find(int n) {
        int p = parents.get(n);
        if (n == p) {
            return n;
        }
        p = find(p);
        parents.put(n, p);
        return p;
    }

    private int find(V i) {
        return find(getNode(i));
    }

    public boolean connected(V i1, V i2) {
        return find(i1) == find(i2);
    }

    public void union(V i1, V i2) {
        int r1 = find(i1);
        int r2 = find(i2);

        if (r1 == r2) {
            return;
        }

        int rank1 = ranks.get(r1);
        int rank2 = ranks.get(r2);
        if (rank1 > rank2) {
            parents.put(r2, r1);
        } else if (rank1 < rank2) {
            parents.put(r1, r2);
        } else {
            parents.put(r2, r1);
            ranks.adjustValue(r1, 1);
        }
    }

    public Collection<Set<V>> connectedComponents() {
        TIntObjectHashMap<Set<V>> components = new TIntObjectHashMap<Set<V>>();
        TObjectIntIterator<V> iter = nodes.iterator();
        for (int i = nodes.size(); i-- > 0;) {
            iter.advance();
            V key = iter.key();
            int group = find(iter.value());
            Set<V> component = components.get(group);
            if (component == null) {
                component = new HashSet<V>();
                components.put(group, component);
            }
            component.add(key);
        }
        return components.valueCollection();
    }

    @Override
    public String toString() {
        return connectedComponents().toString();
    }
}
