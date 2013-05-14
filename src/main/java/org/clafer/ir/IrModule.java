package org.clafer.ir;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrModule {

    private final List<IrBoolVar> boolVars;
    private final List<IrIntVar> intVars;
    private final List<IrSetVar> setVars;
    private final List<IrConstraint> constraints;

    private IrModule(List<IrBoolVar> boolVars, List<IrIntVar> intVars, List<IrSetVar> setVars, List<IrConstraint> constraints) {
        this.boolVars = boolVars;
        this.intVars = intVars;
        this.setVars = setVars;
        this.constraints = constraints;
    }

    public IrModule() {
        this(new ArrayList<IrBoolVar>(), new ArrayList<IrIntVar>(), new ArrayList<IrSetVar>(), new ArrayList<IrConstraint>());
    }

    public void addBoolVar(IrBoolVar var) {
        boolVars.add(Check.notNull(var));
    }

    public void addBoolVars(IrBoolVar[] vars) {
        addBoolVars(Arrays.asList(vars));
    }

    public void addBoolVars(Collection<IrBoolVar> vars) {
        boolVars.addAll(vars);
    }

    public IrBoolVar[] getBoolVars() {
        return boolVars.toArray(new IrBoolVar[boolVars.size()]);
    }

    public void addIntVar(IrIntVar var) {
        intVars.add(Check.notNull(var));
    }

    public void addIntVars(IrIntVar[] vars) {
        addIntVars(Arrays.asList(vars));
    }

    public void addIntVars(Collection<IrIntVar> var) {
        intVars.addAll(var);
    }

    public IrIntVar[] getIntVars() {
        return intVars.toArray(new IrIntVar[intVars.size()]);
    }

    public void addSetVar(IrSetVar var) {
        setVars.add(Check.notNull(var));
    }

    public void addSetVars(IrSetVar[] vars) {
        addSetVars(Arrays.asList(vars));
    }

    public void addSetVars(Collection<IrSetVar> var) {
        setVars.addAll(var);
    }

    public IrSetVar[] getSetVars() {
        return setVars.toArray(new IrSetVar[setVars.size()]);
    }

    public void addConstraint(IrConstraint constraint) {
        if (!IrUtil.isTrue(constraint)) {
            constraints.add(Check.notNull(constraint));
        }
    }

    public void addConstraint(IrBoolExpr expr) {
        if (expr instanceof IrAnd) {
            IrAnd and = (IrAnd) expr;
            for (IrBoolExpr operand : and.getOperands()) {
                addConstraint(operand);
            }
        } else if (!IrUtil.isTrue(expr)) {
            constraints.add(Irs.boolConstraint(Check.notNull(expr)));
        }
    }

    public List<IrConstraint> getConstraints() {
        return constraints;
    }

    public IrModule withConstraints(Collection<IrConstraint> constraints) {
        return new IrModule(boolVars, intVars, setVars, new ArrayList<IrConstraint>(constraints));
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("IrModule").append('\n');
        for (IrConstraint constraint : constraints) {
            result.append("--").append(constraint).append('\n');
        }
        return result.toString();
    }
}
