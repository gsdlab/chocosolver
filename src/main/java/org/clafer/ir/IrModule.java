package org.clafer.ir;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.common.Check;

/**
 * The compiled model in IR. A module contains variables and constraints. The IR
 * is permitted to throw away any variables during the optimization.
 *
 * @author jimmy
 */
public class IrModule {

    private final Set<IrVar> variables;
    private final List<IrBoolExpr> constraints;

    public IrModule() {
        this(100, 100);
    }

    public IrModule(int initialVariableCapacity, int initialConstraintCapacity) {
        this.variables = new HashSet<>(initialVariableCapacity);
        this.constraints = new ArrayList<>(initialConstraintCapacity);
    }

    public IrModule addVariable(IrVar var) {
        if (!(var instanceof IrConstant)) {
            if (var instanceof IrStringVar) {
                IrStringVar string = (IrStringVar) var;
                addVariable(string.getLengthVar());
                addVariables(string.getCharVars());
            }
            variables.add(var);
        }
        return this;
    }

    public IrModule addVariables(IrVar... vars) {
        for (IrVar var : vars) {
            addVariable(var);
        }
        return this;
    }

    public IrModule addVariables(Iterable<? extends IrVar> vars) {
        for (IrVar var : vars) {
            addVariable(var);
        }
        return this;
    }

    public Set<IrVar> getVariables() {
        return Collections.unmodifiableSet(variables);
    }

    public IrModule addConstraint(IrBoolExpr expr) {
        Check.notNull(expr);
        if (expr instanceof IrAnd) {
            IrAnd and = (IrAnd) expr;
            for (IrBoolExpr operand : and.getOperands()) {
                addConstraint(operand);
            }
        } else if (expr instanceof IrNotImplies) {
            IrNotImplies notImplies = (IrNotImplies) expr;
            addConstraint(notImplies.getAntecedent());
            addConstraint(Irs.not(notImplies.getConsequent()));
        } else if (!IrUtil.isTrue(expr)) {
            constraints.add(expr);
        }
        return this;
    }

    public IrModule addConstraints(IrBoolExpr... exprs) {
        for (IrBoolExpr expr : exprs) {
            addConstraint(expr);
        }
        return this;
    }

    public IrModule addConstraints(Iterable<? extends IrBoolExpr> exprs) {
        for (IrBoolExpr expr : exprs) {
            addConstraint(expr);
        }
        return this;
    }

    public List<IrBoolExpr> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("IrModule").append('\n');
        for (IrVar variable : variables) {
            result.append("++").append(variable).append('\n');
        }
        for (IrBoolExpr constraint : constraints) {
            result.append("--").append(constraint).append('\n');
        }
        return result.toString();
    }
}
