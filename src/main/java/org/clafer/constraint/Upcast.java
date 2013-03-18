package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractMixedSetIntSConstraint;
import choco.kernel.solver.variables.Var;
import choco.kernel.solver.variables.set.SetVar;
import org.clafer.Util;

/**
 * Assumption: the domains of the variables are small
 * Assumption: offset is non-negative
 * 
 * @author jimmy
 */
public class Upcast extends AbstractMixedSetIntSConstraint {

    private final SetVar from, to;
    private final int offset;

    public Upcast(SetVar from, SetVar to, int offset) {
        super(new Var[]{from.getCard(), to.getCard(), from, to});
        this.from = from;
        this.offset = offset;
        this.to = to;
    }

    private boolean isFromCardVar(int varIdx) {
        return varIdx == 0;
    }

    private boolean isToCardVar(int varIdx) {
        return varIdx == 1;
    }

    private boolean isFromVar(int varIdx) {
        return varIdx == 2;
    }

    private boolean isToVar(int varIdx) {
        return varIdx == 3;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        if (isFromCardVar(idx) || isToCardVar(idx)) {
            return IntVarEvent.REMVAL_MASK;
        }
        assert isFromVar(idx) || isToVar(idx);
        return SetVarEvent.REMENV_MASK + SetVarEvent.ADDKER_MASK;
    }

    @Override
    public void awakeOnKer(int varIdx, int x) throws ContradictionException {
        if (isFromVar(varIdx)) {
            // Pick to
            to.addToKernel(x + offset, this, false);
        } else {
            // Pick from
            assert isToVar(varIdx);
            from.addToKernel(x - offset, this, false);
        }
    }

    @Override
    public void awakeOnInst(int varIdx) throws ContradictionException {
        // Do nothing
    }

    @Override
    public void awakeOnRem(int varIdx, int val) throws ContradictionException {
        if (isFromCardVar(varIdx)) {
            to.getCard().removeVal(val, this, false);
        } else if (isToCardVar(varIdx)) {
            from.getCard().removeVal(val, this, false);
        } else {
            assert isFromVar(varIdx) || isToVar(varIdx);
            awakeOnEnv(varIdx, val);
        }
    }

    @Override
    public void awakeOnEnv(int varIdx, int x) throws ContradictionException {
        if (isFromVar(varIdx)) {
            // Prune to
            to.remFromEnveloppe(x + offset, this, false);
        } else {
            assert isToVar(varIdx);
            // Prune from
            from.remFromEnveloppe(x - offset, this, false);
        }
    }

    @Override
    public void propagate() throws ContradictionException {
        Util.subsetOf(this, from.getCard(), to.getCard());
        Util.subsetOf(this, to.getCard(), from.getCard());
        pruneFromEnv();
        pruneToEnv();
        pickFromKer();
        pickToKer();
    }

    private void pickFromKer() throws ContradictionException {
        DisposableIntIterator it = to.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                from.addToKernel(it.next() - offset, this, false);
            }
        } finally {
            it.dispose();
        }
    }

    private void pickToKer() throws ContradictionException {
        DisposableIntIterator it = from.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                to.addToKernel(it.next() + offset, this, false);
            }
        } finally {
            it.dispose();
        }
    }

    private void pruneFromEnv() throws ContradictionException {
        DisposableIntIterator it = from.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (!to.isInDomainEnveloppe(val + offset)) {
                    from.remFromEnveloppe(val, this, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    private void pruneToEnv() throws ContradictionException {
        DisposableIntIterator it = to.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (!from.isInDomainEnveloppe(val - offset)) {
                    to.remFromEnveloppe(val, this, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    @Override
    public boolean isSatisfied() {
        DisposableIntIterator ift = from.getDomain().getKernelIterator();
        try {
            while (ift.hasNext()) {
                if (!to.isInDomainKernel(ift.next() + offset)) {
                    return false;
                }
            }
        } finally {
            ift.dispose();
        }

        return from.getKernelDomainSize() == to.getKernelDomainSize();
    }
}
