package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractMixedSetIntSConstraint;
import choco.kernel.solver.variables.Var;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetDomain;
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
    private final IntDomainVar offset;

    public Upcast(SetVar from, IntDomainVar offset, SetVar to) {
        super(new Var[]{offset, from.getCard(), to.getCard(), from, to});
        this.from = from;
        this.offset = offset;
        this.to = to;
    }

    private boolean isOffsetVar(int varIdx) {
        return varIdx == 0;
    }

    private boolean isFromCardVar(int varIdx) {
        return varIdx == 1;
    }

    private boolean isToCardVar(int varIdx) {
        return varIdx == 2;
    }

    private boolean isFromVar(int varIdx) {
        return varIdx == 3;
    }

    private boolean isToVar(int varIdx) {
        return varIdx == 4;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        if (isOffsetVar(idx) || isFromCardVar(idx) || isToCardVar(idx)) {
            return IntVarEvent.REMVAL_MASK + IntVarEvent.INSTINT_MASK;
        }
        return SetVarEvent.REMENV_MASK + SetVarEvent.ADDKER_MASK;
    }

    @Override
    public void awakeOnKer(int varIdx, int x) throws ContradictionException {
        if (isFromVar(varIdx)) {
            pickToKer();
            pruneFromEnv();
        } else {
            pickFromKer();
            pruneToEnv();
        }
        pruneOffset();
        
        assert from.getKernelDomainSize() <= to.getEnveloppeDomainSize();
        assert to.getKernelDomainSize() <= from.getEnveloppeDomainSize();
        assert from.getEnveloppeInf() <= to.getEnveloppeInf();
        assert from.getEnveloppeSup() <= to.getEnveloppeSup();
    }

    
    @Override
    public void awakeOnEnv(int varIdx, int x) throws ContradictionException {
        if (isFromVar(varIdx)) {
            pruneToEnv();
        } else {
            pruneFromEnv();
        }
        pruneOffset();
        
        assert from.getKernelDomainSize() <= to.getEnveloppeDomainSize();
        assert to.getKernelDomainSize() <= from.getEnveloppeDomainSize();
        assert from.getEnveloppeInf() <= to.getEnveloppeInf();
        assert from.getEnveloppeSup() <= to.getEnveloppeSup();
    }

    @Override
    public void awakeOnInst(int varIdx) throws ContradictionException {
        if(isFromCardVar(varIdx)) {
            to.getCard().instantiate(from.getCard().getVal(), this, false);
        } else if(isToCardVar(varIdx)) {
            from.getCard().instantiate(to.getCard().getVal(), this, false);
        }
        
        assert from.getKernelDomainSize() <= to.getEnveloppeDomainSize();
        assert to.getKernelDomainSize() <= from.getEnveloppeDomainSize();
        assert from.getEnveloppeInf() <= to.getEnveloppeInf();
        assert from.getEnveloppeSup() <= to.getEnveloppeSup();
    }

    @Override
    public void awakeOnRem(int varIdx, int val) throws ContradictionException {
        if (isOffsetVar(varIdx)) {
            pickFromKer();
            pickToKer();
            pruneFromEnv();
            pruneToEnv();
        } else if (isFromCardVar(varIdx)) {
            to.getCard().removeVal(val, this, false);
        } else if (isToCardVar(varIdx)) {
            from.getCard().removeVal(val, this, false);
        } else {
            awakeOnEnv(varIdx, val);
        }
        
        assert from.getKernelDomainSize() <= to.getEnveloppeDomainSize();
        assert to.getKernelDomainSize() <= from.getEnveloppeDomainSize();
        assert from.getEnveloppeInf() <= to.getEnveloppeInf();
        assert from.getEnveloppeSup() <= to.getEnveloppeSup();
    }

    @Override
    public void awake() throws ContradictionException {
        offset.updateInf(0, this, false);
        propagate();
    }

    @Override
    public void propagate() throws ContradictionException {
        Util.subsetOf(this, from.getCard(), to.getCard());
        Util.subsetOf(this, to.getCard(), from.getCard());
        pickFromKer();
        pickToKer();
        pruneFromEnv();
        pruneToEnv();
        pruneOffset();

        assert from.getKernelDomainSize() <= to.getEnveloppeDomainSize();
        assert to.getKernelDomainSize() <= from.getEnveloppeDomainSize();
        assert from.getEnveloppeInf() <= to.getEnveloppeInf();
        assert from.getEnveloppeSup() <= to.getEnveloppeSup();
    }

    private void pruneOffset() throws ContradictionException {
        if (from.getKernelDomainSize() > 0 || to.getKernelDomainSize() > 0) {
            int diff = to.getEnveloppeSup() - from.getEnveloppeInf();
            offset.updateSup(diff, this, false);
        }
        if(to.getKernelDomainSize() > 0) {
            int diff = to.getKernelSup() - from.getEnveloppeSup();
            if(diff > 0) {
                offset.updateInf(diff, this, false);
            }
        }
        if(from.getKernelDomainSize() > 0) {
            int diff = to.getEnveloppeInf() - from.getKernelInf();
            if(diff > 0) {
                offset.updateInf(diff, this, false);
            }
        }

        int[] fromKer = Util.iterateKer(from.getDomain());
        int[] toKer = Util.iterateKer(to.getDomain());

        DisposableIntIterator it = offset.getDomain().getIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (!fromKerSupported(fromKer, val)) {
                    offset.removeVal(val, this, false);
                } else if (!toKerSupported(toKer, val)) {
                    offset.removeVal(val, this, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    private void pickFromKer() throws ContradictionException {
        if (offset.isInstantiated()) {
            DisposableIntIterator it = to.getDomain().getKernelIterator();
            try {
                while (it.hasNext()) {
                    from.addToKernel(it.next() - offset.getVal(), this, false);
                }
            } finally {
                it.dispose();
            }
        }
    }

    private void pickToKer() throws ContradictionException {
        if (offset.isInstantiated()) {
            DisposableIntIterator it = from.getDomain().getKernelIterator();
            try {
                while (it.hasNext()) {
                    int v = it.next() + offset.getVal();
                    to.addToKernel(v, this, false);
                }
            } finally {
                it.dispose();
            }
        }
    }

    private static int[] kerArrayPlusEmpty(SetDomain dom) {
        int[] array = new int[dom.getKernelDomain().getSize() + 1];

        DisposableIntIterator it = dom.getKernelIterator();
        try {
            int i = 0;
            while (it.hasNext()) {
                array[++i] = it.next();
            }

            assert (i == array.length - 1);
        } finally {
            it.dispose();
        }

        return array;
    }

    private boolean fromKerSupported(int[] ker, int[] off) {
        for (int o : off) {
            if (fromKerSupported(ker, o)) {
                return true;
            }
        }
        return false;
    }

    private boolean fromKerSupported(int[] ker, int off) {
        for (int k : ker) {
            if (!to.isInDomainEnveloppe(k + off)) {
                return false;
            }
        }
        return true;
    }

    private boolean toKerSupported(int[] ker, int[] off) {
        for (int o : off) {
            if (toKerSupported(ker, o)) {
                return true;
            }
        }
        return false;
    }

    private boolean toKerSupported(int[] ker, int off) {
        for (int k : ker) {
            if (!from.isInDomainEnveloppe(k - off)) {
                return false;
            }
        }
        return true;
    }
    int c = 0;

    private void pruneFromEnv() throws ContradictionException {
        int[] ker = kerArrayPlusEmpty(from.getDomain());
        int[] off = Util.iterateDomain(offset.getDomain());
        c++;
        DisposableIntIterator it = from.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (!from.isInDomainKernel(val)) {
                    ker[0] = val;
                    if (!fromKerSupported(ker, off)) {
                        from.remFromEnveloppe(val, this, false);
                    }
                }
            }
        } finally {
            it.dispose();
        }
    }

    private void pruneToEnv() throws ContradictionException {
        int[] ker = kerArrayPlusEmpty(to.getDomain());
        int[] off = Util.iterateDomain(offset.getDomain());
        c++;
        DisposableIntIterator it = to.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (!to.isInDomainKernel(val)) {
                    ker[0] = val;
                    if (!toKerSupported(ker, off)) {
                        to.remFromEnveloppe(val, this, false);
                    }
                }
            }
        } finally {
            it.dispose();
        }
    }

    @Override
    public boolean isSatisfied() {
        int off = offset.getVal();

        DisposableIntIterator ift = from.getDomain().getKernelIterator();
        try {
            while (ift.hasNext()) {
                if (!to.isInDomainKernel(ift.next() + off)) {
                    return false;
                }
            }
        } finally {
            ift.dispose();
        }

        return from.getKernelDomainSize() == to.getKernelDomainSize();
    }
}
