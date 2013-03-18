package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.integer.AbstractLargeIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;

/**
 *
 * @author jimmy
 */
public class Increasing extends AbstractLargeIntSConstraint {

    public Increasing(IntDomainVar[] vars) {
        super(vars);
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return IntVarEvent.DECSUP_MASK + IntVarEvent.INCINF_MASK;
    }

    @Override
    public void awakeOnInf(int varIdx) throws ContradictionException {
        pruneInf(varIdx);
    }

    @Override
    public void awakeOnSup(int varIdx) throws ContradictionException {
        pruneSup(varIdx);
    }

    @Override
    public void propagate() throws ContradictionException {
        pruneInf(0);
        pruneSup(vars.length - 1);
    }

    private void pruneInf(int varIdx) throws ContradictionException {
        for (int i = varIdx + 1; i < vars.length; i++) {
            if (vars[i - 1].getInf() > vars[i].getInf()) {
                vars[i].updateInf(vars[i - 1].getInf(), this, false);
            }
        }
    }

    private void pruneSup(int varIdx) throws ContradictionException {
        for (int i = varIdx; i > 0; i--) {
            if (vars[i].getSup() < vars[i - 1].getSup()) {
                vars[i - 1].updateSup(vars[i].getSup(), this, false);
            }
        }
    }

    @Override
    public boolean isSatisfied(int[] tuple) {
        for (int i = 1; i < tuple.length; i++) {
            if (tuple[i - 1] > tuple[i]) {
                return false;
            }
        }
        return true;
    }
}
