package org.clafer.compiler;

import java.util.Set;
import org.clafer.ast.AstConstraint;
import org.clafer.instance.InstanceModel;

/**
 *
 * @author jimmy
 */
public class ReachedLimitBestKnownUnsatException extends ReachedLimitException {

    private final Set<AstConstraint> unsat;
    private final InstanceModel counterExample;

    public ReachedLimitBestKnownUnsatException(Set<AstConstraint> unsat, InstanceModel counterExample) {
        this.unsat = unsat;
        this.counterExample = counterExample;
    }

    public Set<AstConstraint> getUnsat() {
        return unsat;
    }

    public InstanceModel getCounterExample() {
        return counterExample;
    }
}
