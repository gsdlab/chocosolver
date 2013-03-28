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

    private final List<IrBoolVar> boolVars = new ArrayList<IrBoolVar>();
    private final List<IrIntVar> intVars = new ArrayList<IrIntVar>();
    private final List<IrSetVar> setVars = new ArrayList<IrSetVar>();
    private final List<IrConstraint> constraints = new ArrayList<IrConstraint>();

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
        constraints.add(Check.notNull(constraint));
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
