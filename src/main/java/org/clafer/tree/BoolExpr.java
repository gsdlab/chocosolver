package org.clafer.tree;

import choco.kernel.model.constraints.Constraint;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class BoolExpr extends Expr {

    private final Constraint value;

    public BoolExpr(Constraint value, List<Constraint> constraints) {
        super(constraints);
        this.value = Check.notNull(value);
    }

    public BoolExpr(Constraint value) {
        this(value, Collections.<Constraint>emptyList());
    }

    public Constraint getValue() {
        return value;
    }
}
