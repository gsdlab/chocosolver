package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractMixedSetIntSConstraint;
import choco.kernel.solver.variables.Var;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetVar;
import org.clafer.Util;

/**
 *
 * @author jimmy
 */
public class BoolChannel extends AbstractMixedSetIntSConstraint {

    private final IntDomainVar[] bools;
    private final SetVar set;

    public BoolChannel(IntDomainVar[] bools, SetVar set) {
        super(init(bools, set));
        this.bools = bools;
        this.set = set;
    }

    private static Var[] init(IntDomainVar[] bools, SetVar set) {
        Var[] vars = new Var[bools.length + 1];
        vars[0] = set;
        System.arraycopy(bools, 0, vars, 1, bools.length);
        return vars;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        if (isSetVar(idx)) {
            return SetVarEvent.ADDKER_MASK + SetVarEvent.REMENV_MASK;
        }
        return IntVarEvent.INSTINT_MASK;
    }

    private boolean isSetVar(int vardIdx) {
        return vardIdx == 0;
    }

    private boolean isBoolsVar(int varIdx) {
        return varIdx > 0;
    }

    private int getBoolsVarIndex(int varIdx) {
        return varIdx - 1;
    }

    @Override
    public void awake() throws ContradictionException {
        for (int i = 0; i < bools.length; i++) {
            if (!set.isInDomainEnveloppe(i)) {
                bools[i].instantiate(0, this, false);
            }
        }
        DisposableIntIterator it = set.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (val < 0 || val >= bools.length) {
                    set.remFromEnveloppe(val, this, false);
                }
            }
        } finally {
            it.dispose();
        }
        propagate();
    }

    @Override
    public void propagate() throws ContradictionException {
        for (int i = 0; i < bools.length; i++) {
            // Pick set
            if (bools[i].isInstantiated()) {
                if (bools[i].getVal() == 0) {
                    set.remFromEnveloppe(i, this, false);
                } else {
                    set.addToKernel(i, this, false);
                }
            }
            // Pick bool
            if (!set.isInDomainEnveloppe(i)) {
                bools[i].instantiate(0, this, false);
            } else if (set.isInDomainKernel(i)) {
                bools[i].instantiate(1, this, false);
            }
        }
    }

    @Override
    public void awakeOnInst(int varIdx) throws ContradictionException {
        // Pick set
        if (isBoolsVar(varIdx)) {
            int id = getBoolsVarIndex(varIdx);
            if (bools[id].getVal() == 0) {
                set.remFromEnveloppe(id, this, false);
            } else {
                set.addToKernel(id, this, false);
            }
        }
    }

    @Override
    public void awakeOnEnv(int varIdx, int x) throws ContradictionException {
        bools[x].instantiate(0, this, false);
    }

    @Override
    public void awakeOnKer(int varIdx, int x) throws ContradictionException {
        bools[x].instantiate(1, this, false);
    }

    @Override
    public boolean isSatisfied() {
        for (int i = 0; i < bools.length; i++) {
            if ((bools[i].getVal() == 0) == set.isInDomainKernel(i)) {
                return false;
            }
        }
        return true;
    }
}
