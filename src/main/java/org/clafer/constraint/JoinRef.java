package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractLargeSetIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetVar;
import gnu.trove.TIntHashSet;
import org.clafer.Check;
import org.clafer.Util;

/**
 * Assumptions: Take set is over a small domain. Ref.length is small.
 * 
 * @author jimmy
 * 
 * TODO: Can perform poorly when branching on integers before sets.
 * For example, if all refs take on distinct values and |take|=4 and
 * |to|=1 then it does not know that it is infeasible and stop early.
 * Builtin setUnion suffers from the same problem.
 */
public class JoinRef extends AbstractLargeSetIntSConstraint {

    private final SetVar take;
    private final IntDomainVar[] refs;
    private final SetVar to;

    public JoinRef(SetVar take, IntDomainVar[] refs, SetVar to) {
        super(Util.cons(take.getCard(), Util.cons(to.getCard(), Check.noNulls(refs))), new SetVar[]{Check.notNull(take), Check.notNull(to)});
        this.take = take;
        this.refs = refs;
        this.to = to;
    }

    boolean isTake(int idx) {
        return idx == 0;
    }

    boolean isTo(int idx) {
        return idx == 1;
    }

    boolean isTakeCard(int idx) {
        return idx == 2;
    }

    boolean isToCard(int idx) {
        return idx == 3;
    }

    boolean isRefsVar(int idx) {
        return idx >= 4;
    }

    int getRefsVarIndex(int idx) {
        return idx - 4;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        if (isIntVarIndex(idx)) {
            return IntVarEvent.REMVAL_MASK + IntVarEvent.INSTINT_MASK;
        }
        return SetVarEvent.ADDKER_MASK + SetVarEvent.REMENV_MASK;
    }

//    @Override
//    public void awakeOnKer(int varIdx, int x) throws ContradictionException {
//        if (isTake(varIdx)) {
//            // pick to
//            if (refs[x].isInstantiated()) {
//                to.addToKernel(refs[x].getVal(), this, false);
//            }
//            // TODO: Do not check all kers
//            pruneRefs();
//        }
//        toEnvEqualsToKern();
//        maxSameAsRefs();
//    }
//
//    @Override
//    public void awakeOnEnv(int varIdx, int x) throws ContradictionException {
//        if (isTake(varIdx)) {
//            pruneTo();
//            toEnvEqualsToKern();
//            maxSameAsRefs();
//        } else {
//            pruneRefs();
//        }
//    }
//
//    @Override
//    public void awakeOnInst(int varIdx) throws ContradictionException {
//        if (isRefsVar(varIdx)) {
//            int idx = getRefsVarIndex(varIdx);
//            // pick to
//            if (take.isInDomainKernel(idx)) {
//                to.addToKernel(refs[idx].getVal(), this, false);
//            }
//            maxSameAsRef(idx);
//            checkEntailed();
//        } else if (isTake(varIdx)) {
//            checkEntailed();
//        }
//    }
//
//    @Override
//    public void awakeOnRem(int x, int v) throws ContradictionException {
//        if (isSetVarIndex(x)) {
//            awakeOnEnv(x, v);
//        } else if (isTakeCard(x)) {
//            to.getCard().updateSup(take.getCard().getSup(), this, false);
//        } else if (isToCard(x)) {
//            take.getCard().updateInf(to.getCard().getInf(), this, false);
//        } else {
////            int y = getRefsVarIndex(x);
////            if (take.isInDomainEnveloppe(y) && !canTakeRefsHaveValue(v)) {
////                to.remFromEnveloppe(v, this, false);
////            }
//            // TODO:
//            propagate();
//        }
//    }
//
//    /**
//     * @return x in (union_{i in env(take)} dom(ref[i]))
//     */
//    private boolean canTakeRefsHaveValue(int x) {
//        DisposableIntIterator it = take.getDomain().getEnveloppeIterator();
//        try {
//            while (it.hasNext()) {
//                int y = it.next();
//                if (refs[y].canBeInstantiatedTo(x)) {
//                    return true;
//                }
//            }
//        } finally {
//            it.dispose();
//        }
//        return false;
//    }
    @Override
    public void awake() throws ContradictionException {
        DisposableIntIterator it = take.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();
                if (x < 0 || x >= refs.length) {
                    take.remFromEnveloppe(x, this, false);
                }
            }
        } finally {
            it.dispose();
        }
        propagate();
    }

    @Override
    public void propagate() throws ContradictionException {
        pruneTo();
        if (!toEnvEqualsToKern()) {
            pickTo();
        }
//        TODO
//        can be tighter depending on ref overlap
        take.getCard().updateInf(to.getCard().getInf(), this, false);
        to.getCard().updateSup(take.getCard().getSup(), this, false);
        pruneRefs();
        maxSameAsRefs();
        pruneTake();
        checkEntailed();
    }

    /**
     *
     */
    private void checkEntailed() throws ContradictionException {
        if (take.isInstantiated() && allRefsInstantiated()) {
            TIntHashSet ans = new TIntHashSet(take.getKernelDomainSize());
            for (int x : take.getValue()) {
                if (x >= 0 && x < refs.length) {
                    int xto = refs[x].getVal();
                    ans.add(xto);
                }
            }
            to.instantiate(ans.toArray(), this, false);
        }
    }

    private boolean allRefsInstantiated() {
        DisposableIntIterator it = take.getDomain().getKernelIterator();

        try {
            while (it.hasNext()) {
                if (!refs[it.next()].isInstantiated()) {
                    return false;
                }
            }
        } finally {
            it.dispose();
        }
        return true;
    }

    private void maxSameAsRefs() throws ContradictionException {
        for (int i = 0; i < refs.length; i++) {
            if (refs[i].isInstantiated()) {
                maxSameAsRef(i);
            }
        }
    }

    private void maxSameAsRef(int y) throws ContradictionException {
        assert (refs[y].isInstantiated());
        if (take.isInDomainKernel(y)) {
            int maxsame = take.getEnveloppeDomainSize() - to.getKernelDomainSize();
            int z = refs[y].getVal();
            if (hasAtLeastXSameRef(maxsame)) {
                DisposableIntIterator it = take.getDomain().getKernelIterator();
                try {
                    while (it.hasNext()) {
                        int x = it.next();

                        if (x != y && !refs[x].isInstantiated()) {
                            refs[x].removeVal(z, this, false);
                        }
                    }
                } finally {
                    it.dispose();
                }
            }
        }
    }

    private boolean hasAtLeastXSameRef(int maxsame) {
        TIntHashSet same = new TIntHashSet(take.getKernelDomainSize());

        DisposableIntIterator it = take.getDomain().getKernelIterator();
        try {
            while (maxsame > 0 && it.hasNext()) {
                int x = it.next();

                if (refs[x].isInstantiated() && !same.add(refs[x].getVal())) {
                    maxsame--;
                }
            }
        } finally {
            it.dispose();
        }
        return maxsame == 0;
    }

    /**
     * Instantiate the to variable to its envelope if that's the only possibility.
     */
    private boolean toEnvEqualsToKern() throws ContradictionException {
        if (to.getKernelDomainSize() + sameRefs() >= take.getEnveloppeDomainSize()) {
            to.instantiate(Util.iterateKer(to), this, false);
            return true;
        }
        return false;
    }

    /**
     * @return the lower bound on the number of refs in the join that are duplicates.
     */
    private int sameRefs() {
        int duplicates = 0;
        TIntHashSet set = new TIntHashSet(take.getKernelDomainSize());

        DisposableIntIterator it = take.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();

                if (refs[val].isInstantiated()) {
                    if (!set.add(refs[val].getVal())) {
                        duplicates++;
                    }
                }
            }
        } finally {
            it.dispose();
        }
        return duplicates;
    }

    // take ker
    // refs dom
    private void pickTo() throws ContradictionException {
        DisposableIntIterator it = take.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (refs[val].isInstantiated()) {
                    to.addToKernel(refs[val].getVal(), this, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    // take env
    // to env (small)
    // refs dom
    private void pruneTo() throws ContradictionException {
        int[] pt = Util.iterateEnv(take);
        DisposableIntIterator it = to.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();

                if (!possibleTo(pt, x)) {
                    to.remFromEnveloppe(x, this, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    // TODO: Can be less conservative for "small" domains.
    private void pruneTake() throws ContradictionException {
        for (int i = 0; i < refs.length; i++) {
            if (!Util.intersects(refs[i], to)) {
                take.remFromEnveloppe(i, this, false);
            }
        }
    }

    // forall i in ker(take), ref_i subset env(to)
    private void pruneRefs() throws ContradictionException {
        DisposableIntIterator it = take.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                int i = it.next();

                Util.subsetOf(this, refs[i], to.getDomain().getEnveloppeDomain());
            }
        } finally {
            it.dispose();
        }
    }

    private boolean possibleTo(int[] take, int to) {
        for (int x : take) {
            if (refs[x].canBeInstantiatedTo(to)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean isSatisfied() {
        TIntHashSet ans = new TIntHashSet(take.getKernelDomainSize());
        for (int x : take.getValue()) {
            if (x >= 0 && x < refs.length) {
                int xto = refs[x].getVal();

                ans.add(xto);
                if (!to.isInDomainKernel(xto)) {
                    return false;
                }
            }
        }
        return ans.size() == to.getValue().length;
    }

    @Override
    public String pretty() {
        return "joinref[" + take.getName() + "]";
    }

    @Override
    public String toString() {
        return pretty();
    }
}
