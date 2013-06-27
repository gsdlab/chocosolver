package org.clafer.ir;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.clafer.common.Check;

/**
 * The compiled model in IR. A module contains variables and constraints. Adding
 * variables is only required for unconstrained variables.
 *
 * @author jimmy
 */
public class IrModule {

    private final List<IrBoolExpr> constraints;

    private IrModule(List<IrBoolExpr> constraints) {
        this.constraints = constraints;
    }

    public IrModule() {
        this(new ArrayList<IrBoolExpr>());
    }

    public IrModule addConstraint(IrBoolExpr expr) {
        Check.notNull(expr);
        if (expr instanceof IrAnd) {
            IrAnd and = (IrAnd) expr;
            for (IrBoolExpr operand : and.getOperands()) {
                addConstraint(operand);
            }
        } else if (!IrUtil.isTrue(expr)) {
            constraints.add(expr);
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

    @Deprecated
    public IrModule withConstraints(IrBoolExpr... constraints) {
        IrModule module = new IrModule(new ArrayList<IrBoolExpr>());
        for (IrBoolExpr constraint : constraints) {
            module.addConstraint(constraint);
        }
        return module;
    }

    @Deprecated
    public IrModule withConstraints(Collection<IrBoolExpr> constraints) {
        IrModule module = new IrModule(new ArrayList<IrBoolExpr>());
        for (IrBoolExpr constraint : constraints) {
            module.addConstraint(constraint);
        }
        return module;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("IrModule").append('\n');
        for (IrBoolExpr constraint : constraints) {
            result.append("--").append(constraint).append('\n');
        }
        return result.toString();
    }
}
