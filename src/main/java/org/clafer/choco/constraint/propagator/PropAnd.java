package org.clafer.choco.constraint.propagator;

import org.clafer.common.Check;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.Variable;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropAnd extends Propagator<Variable> {

    private final Propagator<? extends Variable>[] operands;

    public PropAnd(Propagator<? extends Variable>[] operands) {
        super(new Variable[]{
            operands[0].getSolver().ZERO // Don't know what else to put here.
        }, PropagatorPriority.LINEAR);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.ALL_FINE_EVENTS.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (Propagator<? extends Variable> operand : operands) {
            assert (operand.isReifiedAndSilent());
            System.out.println(operand + " : " + operand.getClass());
            operand.setReifiedTrue();
            operand.propagate(EventType.FULL_PROPAGATION.strengthened_mask);
            solver.getEngine().onPropagatorExecution(operand);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        boolean allTrue = true;
        for (Propagator<? extends Variable> operand : operands) {
            switch (operand.isEntailed()) {
                case FALSE:
                    return ESat.FALSE;
                case UNDEFINED:
                    allTrue = false;
            }
        }
        return allTrue ? ESat.TRUE : ESat.UNDEFINED;
    }
}
