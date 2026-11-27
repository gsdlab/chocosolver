package org.clafer.ontology;

import java.util.HashMap;
import java.util.Map;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;

/**
 *
 * @author jimmy
 */
public class ConstraintDatabase {

    protected final Map<Path, Domain> assignments = new HashMap<>();
    protected final Relation<Path> equalities = new Relation<>();
    protected final Relation<Path> localEqualities = new Relation<>();

    public void newAssignment(Path path, int value) {
        newAssignment(path, Domains.constantDomain(value));
    }

    public ConstraintDatabase newAssignment(Path path, Domain value) {
        assignments.merge(path, value, Domain::intersection);
        return this;
    }

    public ConstraintDatabase newEquality(Path path1, Path path2) {
        equalities.add(path1, path2);
        equalities.add(path2, path1);
        return this;
    }

    public ConstraintDatabase newLocalEquality(Path path1, Path path2) {
        localEqualities.add(path1, path2);
        localEqualities.add(path2, path1);
        return this;
    }
}
