package org.clafer.ontology;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import org.clafer.domain.Domain;
import org.clafer.math.SetTheory;

/**
 * An oracle for answering queries of is-a and has-a relationships.
 *
 * @author jimmy
 */
public class Oracle {

    private final Relation<Concept> isA;
    private final Relation<Concept> hasA;
    private final Relation<Concept> aHas;

    private final IdMap<Path> idMap = new IdMap<>();
    private final SetTheory theory = new SetTheory();
    private int tempId = -1;

    public Oracle(
            Relation<Concept> isA,
            Relation<Concept> hasA,
            Map<Path, Domain> assignments,
            Relation<Path> equalities,
            Relation<Path> localEqualities) {
        this.isA = isA.transitiveClosureWithoutCycles();
        this.hasA = this.isA.compose(hasA);
        this.aHas = this.isA.compose(this.hasA.inverse());

        for (Entry<Path, Domain> assignment : assignments.entrySet()) {
            Path path = assignment.getKey();
            Domain value = assignment.getValue();
            for (Path groundPath : groundPaths(path)) {
                theory.subset(idMap.getId(groundPath), value);
            }
        }

        for (Entry<Path, Set<Path>> equality1 : equalities.entrySet()) {
            Path path1 = equality1.getKey();
            List<Path> groundPaths1 = groundPaths(path1);
            for (Path path2 : equality1.getValue()) {
                int[] groundPaths1Id = idMap.getIds(groundPaths1);
                int[] groundPaths2Id = idMap.getIds(groundPaths(path2));
                unionEqual(groundPaths1Id, groundPaths2Id);
            }
        }

        for (Entry<Path, Set<Path>> equality1 : localEqualities.entrySet()) {
            Path path1 = equality1.getKey();
            List<Path> groundPaths1 = groundPaths(path1);
            Map<Concept, List<Path>> groundPathsMap1
                    = groundPaths1.stream().collect(Collectors.groupingBy(Path::getContext));
            for (Path path2 : equality1.getValue()) {
                Map<Concept, List<Path>> groundPathsMap2
                        = groundPaths(path2).stream().collect(Collectors.groupingBy(Path::getContext));
                for (Concept concept : groundPathsMap1.keySet()) {
                    int[] groundPaths1Id = idMap.getIds(groundPathsMap1.get(concept));
                    int[] groundPaths2Id = idMap.getIds(groundPathsMap2.get(concept));
                    unionEqual(groundPaths1Id, groundPaths2Id);
                }
            }
        }

        new ArrayList<>(idMap.keySet()).forEach(this::addPathConstraints);
    }

    private void unionEqual(int[] union1, int[] union2) {
        if (union1.length == 1) {
            theory.union(union2).equalsTo(union1[0]);
        } else if (union2.length == 1) {
            theory.union(union1).equalsTo(union2[0]);
        } else {
            theory.union(union1).equalsTo(tempId);
            theory.union(union2).equalsTo(tempId);
            tempId--;
        }
    }

    private void addPathConstraints(Path path) {
        for (Path groundPath : groundPaths(path)) {
            Path cur = groundPath;
            while (cur.length() > 1) {
                Path next = cur.dropPrefix(1);
                theory.
                        union(aHas.from(next.getContext()).stream()
                                .filter(x -> isGround(x, isA))
                                .map(next::prepend).mapToInt(idMap::getId).toArray())
                        .equalsTo(idMap.getId(next));
                cur = next;
            }
        }
    }

    public boolean isA(Concept sub, Concept sup) {
        return isA.has(sub, sup);
    }

    public boolean hasA(Concept parent, Concept child) {
        return hasA.has(parent, child);
    }

    public Domain getAssignment(Concept... steps) {
        return getAssignment(new Path(steps));
    }

    /**
     * Returns the envelope of values the path must take.
     *
     * @param path
     * @return the envelope of values the path must take, or null if unbounded
     */
    public Domain getAssignment(Path path) {
        addPathConstraints(path);
        theory
                .union(idMap.getIds(groundPaths(path)))
                .equalsTo(idMap.getId(path));
        theory.propagate();
        return theory.getEnv(idMap.getId(path));
    }

    public void newAssignment(Path path, Domain value) {
        theory.subset(idMap.getId(path), value);
    }

    public boolean propagate() {
        return theory.propagate();
    }

    private ArrayList<Path> groundPaths(Path path) {
        ArrayList<Path> out = new ArrayList<>();
        groundPaths(path.getSteps(), 0, out);
        return out;
    }

    private void groundPaths(Concept[] steps, int index, List<Path> out) {
        if (index == steps.length) {
            out.add(new Path(steps));
        } else {
            Collection<Concept> subs = isA.to(steps[index]);
            for (Concept sub : subs) {
                if (isGround(sub, isA)) {
                    Concept[] alter = steps.clone();
                    alter[index] = sub;
                    groundPaths(alter, index + 1, out);
                }
            }
        }
    }

    private static boolean isGround(Concept concept, Relation<Concept> isA) {
        Set<Concept> subs = isA.to(concept);
        return subs.size() == 1 && subs.contains(concept);
    }
}
