package org.clafer.tree;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.Collections;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class BoolExpr extends Expr {

    private final Constraint value;

    public BoolExpr(Constraint value, List<Constraint> constraints, List<SetVariable> setVars, List<IntegerVariable> intVars) {
        super(constraints, setVars, intVars);
        this.value = Check.notNull(value);
    }

    public BoolExpr(Constraint value) {
        this(value,
                Collections.<Constraint>emptyList(),
                Collections.<SetVariable>emptyList(),
                Collections.<IntegerVariable>emptyList());
    }

    public Constraint getValue() {
        return value;
    }
}
