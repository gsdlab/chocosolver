package org.clafer.tree;

import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author jimmy
 */
public abstract class Expr {

    private final List<Constraint> constraints;
    private final List<SetVariable> setVars;
    private final List<IntegerVariable> intVars;

    public Expr(List<Constraint> constraints, List<SetVariable> setVars, List<IntegerVariable> intVars) {
        this.constraints = new ArrayList<Constraint>(constraints);
        this.setVars = new ArrayList<SetVariable>(setVars);
        this.intVars = new ArrayList<IntegerVariable>(intVars);
    }

    public List<Constraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    public List<SetVariable> getSetVars() {
        return Collections.unmodifiableList(setVars);
    }

    public List<IntegerVariable> getIntVars() {
        return Collections.unmodifiableList(intVars);
    }
}
