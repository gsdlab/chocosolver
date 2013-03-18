package org.clafer.tree;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.set.SetVariable;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class SetExpr extends Expr {

    private final AtomicClafer type;
    private final SetVariable value;

    public SetExpr(AtomicClafer type, SetVariable value, List<Constraint> constraints) {
        super(constraints);
        this.type = Check.notNull(type);
        this.value = Check.notNull(value);
    }

    public SetExpr(AtomicClafer type, SetVariable value) {
        this(type, value, Collections.<Constraint>emptyList());
    }

    public AtomicClafer getType() {
        return type;
    }

    public SetVariable getValue() {
        return value;
    }

    public int getUppB() {
        return value.getUppB();
    }

    public int getLowB() {
        return value.getLowB();
    }
    
    public SetExpr withType(AtomicClafer newType) {
        return new SetExpr(newType, value, getConstraints());
    }
}
