package org.clafer.choco.constraint;

import org.clafer.Util;
import org.clafer.choco.constraint.propagator.PropAnd;
import solver.constraints.Constraint;
import solver.constraints.propagators.Propagator;
import solver.variables.Variable;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class AndConstraint extends Constraint<Variable, Propagator<Variable>> {

    private final Constraint[] operands;

    public AndConstraint(Constraint... operands) {
        super(getVariables(operands), operands[0].getSolver());
        this.operands = operands;
        Propagator[] props = getPropagators(operands);
        PropAnd reifProp = new PropAnd(props);
        props = Util.cons(reifProp, props);
        setPropagators(props);
        for (Propagator prop : props) {
            prop.setReifiedSilent();
        }
    }

    private static Variable[] getVariables(Constraint[] operands) {
        Variable[][] variables = new Variable[operands.length][];
        for (int i = 0; i < variables.length; i++) {
            variables[i] = operands[i].getVariables();
        }
        return Util.concat(variables);
    }

    private static Propagator[] getPropagators(Constraint[] operands) {
        Propagator[][] propagators = new Propagator[operands.length][];
        for (int i = 0; i < propagators.length; i++) {
            propagators[i] = operands[i].getPropagators();
        }
        return Util.concat(propagators);
    }

    @Override
    public ESat isSatisfied() {
        boolean allTrue = true;
        for (Constraint operand : operands) {
            switch (operand.isSatisfied()) {
                case FALSE:
                    return ESat.FALSE;
                case UNDEFINED:
                    allTrue = false;
            }
        }
        return allTrue ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public ESat isEntailed() {
        boolean allTrue = true;
        for (Constraint operand : operands) {
            switch (operand.isEntailed()) {
                case FALSE:
                    return ESat.FALSE;
                case UNDEFINED:
                    allTrue = false;
            }
        }
        return allTrue ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "(" + Util.intercalate(") & (", operands) + ")";
    }
}
