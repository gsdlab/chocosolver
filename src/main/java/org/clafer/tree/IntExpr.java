package org.clafer.tree;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class IntExpr extends Expr {

    private final IntegerVariable value;

    public IntExpr(IntegerVariable value, 
            List<Constraint> constraints, 
            List<SetVariable> setVars,
            List<IntegerVariable> intVars) {
        super(constraints, setVars, intVars);
        this.value = value;
    }

    public IntExpr(IntegerVariable value) {
        this(value, 
                Collections.<Constraint>emptyList(),
                Collections.<SetVariable>emptyList(), 
                Collections.<IntegerVariable>emptyList());
    }

    public IntegerVariable getValue() {
        return value;
    }
}
