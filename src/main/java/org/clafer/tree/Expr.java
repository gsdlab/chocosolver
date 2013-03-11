package org.clafer.tree;

import choco.kernel.model.constraints.Constraint;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class Expr {

    private final List<Constraint> constraints;

    public Expr(List<Constraint> constraints) {
        this.constraints = new ArrayList<Constraint>(Check.noNulls(constraints));
    }

    public List<Constraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }
}
