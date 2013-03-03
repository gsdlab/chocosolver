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
public class SetExpr extends Expr {

    private final SetVariable value;

    public SetExpr(SetVariable value,
            List<Constraint> constraints,
            List<SetVariable> setVars,
            List<IntegerVariable> intVars) {
        super(constraints, setVars, intVars);
        this.value = Check.notNull(value);
    }

    public SetExpr(SetVariable value) {
        this(value,
                Collections.<Constraint>emptyList(),
                Collections.<SetVariable>emptyList(),
                Collections.<IntegerVariable>emptyList());
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
}
