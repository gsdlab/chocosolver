package org.clafer.ontology;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.collection.Pair;
import org.clafer.domain.Domain;

/**
 * An oracle for answering queries of is-a and has-a relationships.
 *
 * @author jimmy
 */
public class Oracle {

    private final Relation<Concept> isA;
    private final Relation<Concept> hasA;
    private final Map<Path, Domain> assignments;
    private final Relation<Path> equalities;

    private final Map<Path, Domain> cache = new HashMap<>();

    public Oracle(
            Relation<Concept> isA,
            Relation<Concept> hasA,
            Map<Path, Domain> assignments,
            Relation<Path> equalities) {
        this.isA = learnInheritedIsA(isA);
        this.hasA = learnInheritedHasA(this.isA, hasA);
        this.assignments = learnAllGroundAssignments(this.isA, assignments);
        this.equalities = learnAllGroundEqualities(this.isA, equalities);
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

    private static Map<Path, Domain> learnAllGroundAssignments(Relation<Concept> isA, Map<Path, Domain> assignments) {
        Map<Path, Domain> newAssignments = new HashMap<>();
        for (Map.Entry<Path, Domain> assignment : assignments.entrySet()) {
            Concept[] steps = assignment.getKey().getSteps();

            for (Concept[] groundPath : groundPaths(steps, isA)) {
                newAssignments.merge(new Path(groundPath), assignment.getValue(), Domain::intersection);
            }
        }
        return newAssignments;
    }

    private static Relation<Path> learnAllGroundEqualities(Relation<Concept> isA, Relation<Path> equalities) {
        Relation<Path> newEqualities = new Relation<>();
        for (Pair<Path, Path> equality : equalities) {
            for (Concept[] groundPath1 : groundPaths(equality.getFst().getSteps(), isA)) {
                for (Concept[] groundPath2 : groundPaths(equality.getSnd().getSteps(), isA)) {
                    if (groundPath1[0].equals(groundPath2[0])) {
                        newEqualities.add(new Path(groundPath1), new Path(groundPath2));
                    }
                }
            }
        }
        return newEqualities;
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
        if (cache.containsKey(path)) {
            return cache.get(path);
        }
        // Temporary cache to avoid infinite recursion.
        cache.put(path, null);
        List<Path> groundPaths = groundAndExpand(path);
        Iterator<Path> iter = groundPaths.iterator();
        assert iter.hasNext();
        Domain domain = getGroundPathAssignment(iter.next());
        while (domain != null && iter.hasNext()) {
            Domain nextDomain = getGroundPathAssignment(iter.next());
            domain = nextDomain == null ? null : domain.union(nextDomain);
        }
        cache.put(path, domain);
        return domain;
    }

    private Domain getGroundPathAssignment(Path groundPath) {
        Domain domain = null;
        for (int i = 0; i < groundPath.length(); i++) {
            // Try all suffixes.
            Path suffix = groundPath.dropPrefix(i);
            Domain groundDomain = getGroundSuffixPathAssignment(suffix);
            if (groundDomain != null) {
                domain = domain == null ? groundDomain : domain.intersection(groundDomain);
            }
        }
        return domain;
    }

    private Domain getGroundSuffixPathAssignment(Path groundPath) {
        assert groundPaths(groundPath.getSteps()).size() == 1 : groundPath;

        Domain assigmentDomain = getGroundSuffixPathDirectAssignment(groundPath);
        Domain equalityDomain = getGroundSuffixPathEqualityAssignment(groundPath);
        if (assigmentDomain == null) {
            return equalityDomain;
        }
        return equalityDomain == null ? assigmentDomain : assigmentDomain.intersection(equalityDomain);
    }

    private Domain getGroundSuffixPathDirectAssignment(Path groundPath) {
        assert groundPaths(groundPath.getSteps()).size() == 1;

        return assignments.get(groundPath);
    }

    private Domain getGroundSuffixPathEqualityAssignment(Path groundPath) {
        assert groundPaths(groundPath.getSteps()).size() == 1;

        Domain domain = null;
        for (Path equality : equalities.from(groundPath)) {
            Domain assignment = getAssignment(equality);
            if (assignment != null) {
                domain = domain == null
                        ? assignment
                        : domain.intersection(assignment);
            }
        }
        return domain;
    }

    private List<Path> groundAndExpand(Path path) {
        List<Path> expandPaths = new ArrayList<>();
        ArrayList<Path> queue = new ArrayList<>();
        queue.add(path);
        while (!queue.isEmpty()) {
            for (Concept[] groundSteps : groundPaths(queue.remove(queue.size() - 1).getSteps())) {
                Path groundPath = new Path(groundSteps);
                Path expandPath = expandPath(groundPath);
                if (groundPath == expandPath) {
                    expandPaths.add(expandPath);
                } else {
                    queue.add(expandPath);
                }
            }
        }
        return expandPaths;
    }

    private Path expandPath(Path path) {
        Concept[] steps = expandPath(path.getSteps());
        if (steps == path.getSteps()) {
            return path;
        }
        return new Path(steps);
    }

    private Concept[] expandPath(Concept[] steps) {
        Concept uniqueParent = highest(hasA.to(steps[0]));
        if (uniqueParent == null) {
            return steps;
        }
        for (Concept step : steps) {
            if (uniqueParent.equals(step)) {
                return steps;
            }
        }
        Concept[] newSteps = new Concept[steps.length + 1];
        newSteps[0] = uniqueParent;
        System.arraycopy(steps, 0, newSteps, 1, steps.length);
        return newSteps;
    }

    private Concept highest(Collection<Concept> concepts) {
        for (Concept concept : concepts) {
            if (concepts.stream().allMatch(x -> isA(x, concept))) {
                return concept;
            }
        }
        return null;
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
