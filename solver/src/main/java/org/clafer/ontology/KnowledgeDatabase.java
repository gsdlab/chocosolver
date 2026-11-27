package org.clafer.ontology;

import java.util.ArrayList;
import java.util.List;

/**
 * Knowledge database containing facts of is-a and has-a relationships.
 *
 * @author jimmy
 */
public class KnowledgeDatabase extends ConstraintDatabase {

    private final Relation<Concept> isA = new Relation<>();
    private final Relation<Concept> hasA = new Relation<>();
    private final List<ConstraintDatabase[]> disjunctions = new ArrayList<>();

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

    public void newDisjunction(ConstraintDatabase... disjunction) {
        if (disjunction.length <= 1) {
            throw new IllegalArgumentException();
        }
        this.disjunctions.add(disjunction);
    }

    public static ConstraintDatabase or() {
        return new ConstraintDatabase();
    }

    public Oracle oracle() {
        return new Oracle(isA, hasA, this, disjunctions);
    }
}
