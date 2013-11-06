package org.clafer.collection;

import gnu.trove.iterator.TObjectIntIterator;
import gnu.trove.list.array.TIntArrayList;
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

    private final TObjectIntHashMap<V> nodes = new TObjectIntHashMap<>(16, 0.5f, -1);
    private final TIntArrayList parents = new TIntArrayList(16);

    private int getNode(V i) {
        int n = nodes.size();
        int v = nodes.putIfAbsent(i, n);
        if (v == -1) {
            parents.add(n);
            assert nodes.size() == parents.size();
            return n;
        }
        return v;
    }

    private int find(int n) {
        int p = parents.get(n);
        if (n == p) {
            return n;
        }
        p = find(p);
        parents.set(n, p);
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

        if (r1 != r2) {
            parents.set(r2, r1);
        }
    }

    public Collection<Set<V>> connectedComponents() {
        TIntObjectHashMap<Set<V>> components = new TIntObjectHashMap<>();
        TObjectIntIterator<V> iter = nodes.iterator();
        for (int i = nodes.size(); i-- > 0;) {
            iter.advance();
            V key = iter.key();
            int group = find(iter.value());
            Set<V> component = components.get(group);
            if (component == null) {
                component = new HashSet<>();
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
