package org.clafer.ontology;

import java.util.HashMap;
import java.util.Map;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;

/**
 * Knowledge database containing facts of is-a and has-a relationships.
 *
 * @author jimmy
 */
public class KnowledgeDatabase {

    private final Relation<Concept> isA = new Relation<>();
    private final Relation<Concept> hasA = new Relation<>();
    private final Map<Path, Domain> assignments = new HashMap<>();
    private final Relation<Path> equalities = new Relation<>();

    public Concept newConcept(String name) {
        Concept concept = new Concept(name);
        newIsA(concept, concept);
        return concept;
    }

    public void newIsA(Concept sub, Concept sup) {
        isA.add(sub, sup);
    }

    public void newHasA(Concept parent, Concept child) {
        hasA.add(parent, child);
    }

    public void newAssignment(Path path, int value) {
        newAssignment(path, Domains.constantDomain(value));
    }

    public void newAssignment(Path path, Domain value) {
        assignments.merge(path, value, Domain::intersection);
    }

    public void newEquality(Path path1, Path path2) {
        equalities.add(path1, path2);
        equalities.add(path2, path1);
    }

    public Oracle oracle() {
        return new Oracle(isA, hasA, assignments, equalities);
    }
}
