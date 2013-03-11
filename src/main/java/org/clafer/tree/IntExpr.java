package org.clafer.tree;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class IntExpr extends Expr {

    private final AtomicClafer type;
    private final IntegerVariable value;

    public IntExpr(AtomicClafer type, IntegerVariable value, List<Constraint> constraints) {
        super(constraints);
        this.type = type;
        this.value = value;
    }

    public IntExpr(AtomicClafer type, IntegerVariable value) {
        this(type, value, Collections.<Constraint>emptyList());
    }

    public AtomicClafer getType() {
        return type;
    }

    public IntegerVariable getValue() {
        return value;
    }

    public int getUppB() {
        return value.getUppB();
    }

    public int getLowB() {
        return value.getLowB();
    }
}
