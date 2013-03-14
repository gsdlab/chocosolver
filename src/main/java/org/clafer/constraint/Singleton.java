package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.AbstractSConstraint;
import choco.kernel.solver.constraints.set.AbstractBinSetIntSConstraint;
import choco.kernel.solver.variables.Var;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetVar;
import org.clafer.Util;

/**
 *
 * @author jimmy
 */
public final class Singleton extends AbstractBinSetIntSConstraint {

    public Singleton(SetVar set, IntDomainVar iv) {
        super(iv, set);
    }

    @Override
    public int getFilteredEventMask(int idx) {
        if (idx == 0) {
            return IntVarEvent.INSTINT_MASK + IntVarEvent.REMVAL_MASK;
        }
        return SetVarEvent.ADDKER_MASK + SetVarEvent.REMENV_MASK + SetVarEvent.INSTSET_MASK;
    }

    @Override
    public void awakeOnRem(int varIdx, int x) throws ContradictionException {
        if(varIdx == 0) {
            v1.remFromEnveloppe(x, this, false);
        } else {
            awakeOnEnv(varIdx, x);
        }
    }

    @Override
    public void awakeOnKer(int varIdx, int x) throws ContradictionException {
        v0.instantiate(x, this, false);
        v1.instantiate(new int[]{x}, this, false);
    }

    @Override
    public void awakeOnEnv(int varIdx, int x) throws ContradictionException {
        v0.removeVal(x, this, false);
    }

    @Override
    public void awakeOnInst(int varIdx) throws ContradictionException {
        if(varIdx == 0) {
            v1.instantiate(new int[]{v0.getVal()}, this, false);
        } else {
            v0.instantiate(v1.getValue()[0], this, false);
        }
    }

    @Override
    public void awake() throws ContradictionException {
        v1.getCard().instantiate(1, this, false);
        propagate();
    }

    @Override
    public void propagate() throws ContradictionException {
        Util.subsetOf(this, v0, v1.getDomain().getEnveloppeDomain());
        Util.envSubsetOf(this, v1, v0);
    }

    @Override
    public boolean isSatisfied() {
        return v1.getValue().length == 1 && v0.getVal() == v1.getValue()[0];
    }

    @Override
    public String toString() {
        return v0 + " = sum(" + v1 + ")";
    }

    @Override
    public String pretty() {
        return v0.pretty() + " = sum(" + v1.pretty() + ")";
    }

    @Override
    public AbstractSConstraint<Var> opposite(Solver solver) {
        return super.opposite(solver);
    }
}
