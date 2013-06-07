package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.BoolVar;
import solver.variables.EventType;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropAnd extends Propagator<BoolVar> {

    public PropAnd(BoolVar[] vars) {
        super(vars, PropagatorPriority.UNARY, true);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.INSTANTIATE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (BoolVar var : vars) {
            var.setToTrue(aCause);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        propagate(mask);
    }

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        for (BoolVar var : vars) {
            if (var.instantiated()) {
                if (var.getValue() == 0) {
                    return ESat.FALSE;
                }
            } else {
                allInstantiated = false;
            }
        }
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return Util.intercalate(" && ", vars);
    }
}
