package org.clafer.ontology;

/**
 *
 * @author jimmy
 */
public class Concept {

    private final String name;

    Concept(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return name;
    }
}
