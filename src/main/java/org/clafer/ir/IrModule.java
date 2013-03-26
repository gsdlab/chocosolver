package org.clafer.ir;

import java.util.ArrayList;
import java.util.List;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrModule {

    private final List<IrConstraint> constraints = new ArrayList<IrConstraint>();

    public void addConstraint(IrConstraint constraint) {
        constraints.add(Check.notNull(constraint));
    }

    public void addConstraint(IrBoolExpr expr) {
        if (expr instanceof IrAnd) {
            IrAnd and = (IrAnd) expr;
            for (IrBoolExpr operand : and.getOperands()) {
                addConstraint(operand);
            }
        } else {
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
