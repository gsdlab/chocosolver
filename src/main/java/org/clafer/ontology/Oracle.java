package org.clafer.ontology;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import org.clafer.collection.Pair;
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

    private final IdMap<Path> idMap = new IdMap<>();
    private final SetTheory theory = new SetTheory();
    int tempId = -1;

    public Oracle(
            Relation<Concept> isA,
            Relation<Concept> hasA,
            Map<Path, Domain> assignments,
            Relation<Path> equalities) {
        this.isA = learnInheritedIsA(isA);
        this.hasA = learnInheritedHasA(this.isA, hasA);

        for (Entry<Path, Domain> assignment : assignments.entrySet()) {
            Concept[] steps = assignment.getKey().getSteps();
            for (Concept[] groundPath : groundPaths(steps, isA)) {
                theory.subset(idMap.getId(new Path(groundPath)), assignment.getValue());
            }
        }

        for (Entry<Path, Set<Path>> equality1 : equalities.entrySet()) {
            Map<Concept, List<Path>> groundPathsMap1
                    = groundPaths(equality1.getKey().getSteps(), isA).stream().map(Path::new)
                    .collect(Collectors.groupingBy(Path::getContext));
            for (Path equality2 : equality1.getValue()) {
                Map<Concept, List<Path>> groundPathsMap2
                        = groundPaths(equality2.getSteps(), isA).stream().map(Path::new)
                        .collect(Collectors.groupingBy(Path::getContext));
                for (Concept concept : groundPathsMap1.keySet()) {
                    int[] groundPaths1 = groundPathsMap1.get(concept).stream().mapToInt(idMap::getId).toArray();
                    int[] groundPaths2 = groundPathsMap2.get(concept).stream().mapToInt(idMap::getId).toArray();
                    if (groundPaths1.length == 1) {
                        theory.union(groundPaths2).equalsTo(groundPaths1[0]);
                    }
                    if (groundPaths2.length == 1) {
                        theory.union(groundPaths1).equalsTo(groundPaths2[0]);
                    }
                    theory.union(groundPaths1).equalsTo(tempId);
                    theory.union(groundPaths2).equalsTo(tempId);
                    tempId--;
                }
            }
        }

        for (Path path : new ArrayList<>(idMap.keySet())) {
            Path cur = path;
            while (cur.length() > 1) {
                Path next = cur.dropPrefix(1);
                theory.
                        union(hasA.to(next.getContext()).stream()
                                .filter(x -> isGround(x, isA))
                                .map(next::prepend).mapToInt(idMap::getId).toArray())
                        .equalsTo(idMap.getId(next));
                cur = next;
            }
        }
    }

    private static Relation<Concept> learnInheritedIsA(Relation<Concept> isA) {
        return isA.transitiveClosureWithoutCycles();
    }

    private static Relation<Concept> learnInheritedHasA(
            Relation<Concept> isA,
            Relation<Concept> hasA) {
        Relation<Concept> newHasA = new Relation<>(hasA);
        for (Entry<Concept, Set<Concept>> entry : isA.entrySet()) {
            Concept concept = entry.getKey();
            Set<Concept> sups = entry.getValue();
            for (Concept sup : sups) {
                newHasA.addAll(concept, newHasA.from(sup));
            }
        }
        return newHasA;
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
        for (Concept[] steps : groundPaths(path.getSteps())) {
            Path cur = new Path(steps);
            while (cur.length() > 1) {
                Path next = cur.dropPrefix(1);
                theory.
                        union(hasA.to(next.getContext()).stream()
                                .filter(x -> isGround(x, isA))
                                .map(next::prepend).mapToInt(idMap::getId).toArray())
                        .equalsTo(idMap.getId(next));
                cur = next;
            }
        }

        theory
                .union(groundPaths(path.getSteps()).stream().map(Path::new).mapToInt(idMap::getId).toArray())
                .equalsTo(idMap.getId(path));
        theory.propagate();
        return theory.getEnv(idMap.getId(path));
    }

    private ArrayList<Concept[]> groundPaths(Concept[] steps) {
        return groundPaths(steps, isA);
    }

    private static ArrayList<Concept[]> groundPaths(Concept[] steps, Relation<Concept> isA) {
        ArrayList<Concept[]> out = new ArrayList<>();
        groundPaths(steps, 0, isA, out);
        return out;
    }

    private static void groundPaths(Concept[] steps, int index, Relation<Concept> isA, List<Concept[]> out) {
        if (index == steps.length) {
            out.add(steps);
        } else {
            Collection<Concept> subs = isA.to(steps[index]);
            for (Concept sub : subs) {
                if (isGround(sub, isA)) {
                    Concept[] alter = steps.clone();
                    alter[index] = sub;
                    groundPaths(alter, index + 1, isA, out);
                }
            }
        }
    }

    private static boolean isGround(Concept concept, Relation<Concept> isA) {
        Set<Concept> subs = isA.to(concept);
        return subs.size() == 1 && subs.contains(concept);
    }
}
