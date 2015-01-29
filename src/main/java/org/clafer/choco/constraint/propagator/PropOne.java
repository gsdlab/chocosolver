package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropOne extends Propagator<BoolVar> {

    public PropOne(BoolVar[] vars) {
        super(vars, PropagatorPriority.BINARY, true);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return IntEventType.instantiation();
    }

    private void clearAllBut(int exclude) throws ContradictionException {
        for (int i = 0; i < vars.length; i++) {
            if (i != exclude) {
                vars[i].setToFalse(aCause);
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // The number of uninstantiated variables.
        int count = 0;
        BoolVar last = null;
        for (int i = 0; i < vars.length; i++) {
            BoolVar var = vars[i];
            if (var.isInstantiated()) {
                if (var.getValue() == 1) {
                    clearAllBut(i);
                    return;
                }
            } else {
                count++;
                last = var;
            }
        }
        // Every variable is false except for last.
        if (count == 1) {
            last.setToTrue(aCause);
        }
        if (count == 0) {
            contradiction(vars[0], "All false.");
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (vars[idxVarInProp].getValue() == 1) {
            clearAllBut(idxVarInProp);
        } else {
            propagate(mask);
        }
    }

    @Override
    public ESat isEntailed() {
        int count = 0;
        boolean allInstantiated = true;
        for (BoolVar var : vars) {
            if (var.isInstantiated()) {
                if (var.getValue() == 1) {
                    count++;
                    if (count > 1) {
                        return ESat.FALSE;
                    }
                }
            } else {
                allInstantiated = false;
            }
        }
        return allInstantiated
                ? (count == 1 ? ESat.TRUE : ESat.FALSE) : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "one(" + Util.commaSeparate(vars) + ")";
    }
}
