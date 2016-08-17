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
                vars[i].setToFalse(this);
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int zeroes = 0;
        int unassigned = 0;
        for (int i = 0; i < vars.length; i++) {
            BoolVar var = vars[i];
            if (var.isInstantiated()) {
                if (var.getValue() == 1) {
                    clearAllBut(i);
                    return;
                } else {
                    zeroes++;
                }
            } else {
                unassigned = i;
            }
        }
        if (zeroes >= vars.length - 1) {
            vars[unassigned].setToTrue(this);
            clearAllBut(unassigned);

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
