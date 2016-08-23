package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class PropLone extends Propagator<BoolVar> {

    public PropLone(BoolVar[] vars) {
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
        for (int i = 0; i < vars.length; i++) {
            BoolVar var = vars[i];
            if (var.isInstantiated()) {
                if (var.getValue() == 1) {
                    clearAllBut(i);
                    return;
                }
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (vars[idxVarInProp].getValue() == 1) {
            clearAllBut(idxVarInProp);
        }
    }

    @Override
    public ESat isEntailed() {
        int countOne = 0;
        int countZero = 0;
        boolean allInstantiated = true;
        for (BoolVar var : vars) {
            if (var.isInstantiated()) {
                if (var.getValue() == 1) {
                    countOne++;
                    if (countOne > 1) {
                        return ESat.FALSE;
                    }
                } else {
                    countZero++;
                }
            }
        }
        return countZero >= vars.length - 1 ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "lone(" + Util.commaSeparate(vars) + ")";
    }
}
