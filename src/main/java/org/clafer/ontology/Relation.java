package org.clafer.ontology;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiConsumer;
import org.clafer.collection.Pair;
import org.clafer.graph.GraphUtil;
import org.clafer.graph.KeyGraph;

/**
 * A binary relation.
 *
 * @param <T> the type
 * @author jimmy
 */
public class Relation<T> implements Iterable<Pair<T, T>> {

    /**
     * Contains the forward joins.
     */
    final Map<T, Set<T>> forwards;
    /**
     * Contains the backward joins.
     */
    final Map<T, Set<T>> backwards;

    public Relation() {
        this(new HashMap<>(), new HashMap<>());
    }

    public Relation(Relation<T> copy) {
        this(new HashMap<>(copy.forwards), new HashMap<>(copy.backwards));
    }

    Relation(Map<T, Set<T>> forwards, Map<T, Set<T>> backwards) {
        this.forwards = forwards;
        this.backwards = backwards;
    }

    public void add(T a, T b) {
        Set<T> bs = forwards.get(a);
        if (bs == null) {
            bs = new HashSet<>();
            forwards.put(a, bs);
        }
        bs.add(b);

        Set<T> as = backwards.get(b);
        if (as == null) {
            as = new HashSet<>();
            backwards.put(b, as);
        }
        as.add(a);
    }

    public void addAll(T a, Collection<T> b) {
        Set<T> bs = forwards.get(a);
        if (bs == null) {
            bs = new HashSet<>();
            forwards.put(a, bs);
        }
        bs.addAll(b);

        for (T t : b) {
            Set<T> as = backwards.get(t);
            if (as == null) {
                as = new HashSet<>();
                backwards.put(t, as);
            }
            as.add(a);
        }
    }

    public void set(Relation<T> relation) {
        forwards.clear();
        forwards.putAll(relation.forwards);
        backwards.clear();
        backwards.putAll(relation.backwards);
    }

    public boolean has(T a, T b) {
        Set<T> bs = forwards.get(a);
        return bs != null && bs.contains(b);
    }

    public Set<T> from(T a) {
        Set<T> bs = forwards.get(a);
        return bs == null ? Collections.emptySet() : bs;
    }

    public Set<T> to(T b) {
        Set<T> as = backwards.get(b);
        return as == null ? Collections.emptySet() : as;
    }

    public Relation<T> compose(Relation<T> with) {
        Relation<T> composition = new Relation<>();
        for (Entry<T, Set<T>> entry : entrySet()) {
            T sub = entry.getKey();
            Set<T> sups = entry.getValue();
            for (T sup : sups) {
                composition.addAll(sub, with.from(sup));
            }
        }
        return composition;
    }

    public Relation<T> inverse() {
        return new Relation<>(new HashMap<>(backwards), new HashMap<>(forwards));
    }

    public void closeTransitively() {
        // Naive algorithm.
        for (Entry<T, Set<T>> forward : forwards.entrySet()) {
            Set<T> reachable = new HashSet<>(forward.getValue().size());
            reachable(forward.getKey(), reachable);
            forward.setValue(reachable);
        }
    }

    private void reachable(T from, Set<T> reachable) {
        Set<T> tos = forwards.get(from);
        if (tos != null) {
            for (T to : tos) {
                if (reachable.add(to)) {
                    reachable(to, reachable);
                }
            }
        }
    }

    public Relation<T> transitiveClosureWithoutCycles() {
        KeyGraph<T> graph = new KeyGraph<>();
        forEach(graph::addEdge);
        List<Set<T>> items = GraphUtil.computeStronglyConnectedComponents(graph);
        for (Set<T> cycle : items) {
            if (cycle.size() > 1) {
                throw new IllegalStateException("Found a cycle in the relationship: " + cycle);
            }
        }
        Map<T, Set<T>> newForwards = new HashMap<>(forwards);
        Map<T, Set<T>> newBackwards = new HashMap<>(backwards);
        for (int i = 0; i < 2; i++) {
            Map<T, Set<T>> map = i == 0 ? newForwards : newBackwards;
            for (Set<T> item : items) {
                for (T from : item) {
                    Set<T> newTos = new HashSet<>();
                    newTos.add(from);
                    Set<T> tos = map.get(from);
                    if (tos != null) {
                        newTos.addAll(tos);
                        tos.stream().map(map::get)
                                .filter(Objects::nonNull)
                                .forEach(newTos::addAll);
                    }
                    map.put(from, newTos);
                }
            }
        }
        return new Relation<>(newForwards, newBackwards);
    }

    public void removeSymmetry() {
        for (Entry<T, Set<T>> forward : forwards.entrySet()) {
            forward.getValue().remove(forward.getKey());
        }
        for (Entry<T, Set<T>> backward : backwards.entrySet()) {
            backward.getValue().remove(backward.getKey());
        }
    }

    public Set<Entry<T, Set<T>>> entrySet() {
        return forwards.entrySet();
    }

    public void forEach(BiConsumer<T, T> action) {
        forwards.forEach((x, y) -> y.forEach(z -> action.accept(x, z)));
    }

    @Override
    public Iterator<Pair<T, T>> iterator() {
        return forwards.entrySet().stream()
                .flatMap(x -> x.getValue().stream().map(y -> new Pair<>(x.getKey(), y)))
                .iterator();
    }
}
