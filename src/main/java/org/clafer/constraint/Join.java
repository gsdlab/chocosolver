package org.clafer.constraint;

import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractLargeSetSConstraint;
import choco.kernel.solver.variables.set.SetVar;
import gnu.trove.TIntHashSet;
import org.clafer.Util;

/**
 * Assumption: Children are disjoint! Does not enforce this, since in the context
 * of Clafer, it's already enforced elsewhere.
 * 
 * @author jimmy
 */
public class Join extends AbstractLargeSetSConstraint {

    private final SetVar take;
    private final SetVar[] children;
    private final SetVar to;

    public Join(SetVar take, SetVar[] children, SetVar to) {
        super(buildArray(take, to, children));
        this.take = take;
        this.children = children;
        this.to = to;
    }

    private static SetVar[] buildArray(SetVar take, SetVar to, SetVar[] children) {
        SetVar[] array = new SetVar[children.length + 2];
        array[0] = take;
        array[1] = to;
        System.arraycopy(children, 0, array, 2, children.length);
        return array;
    }

    private boolean isTake(int varIdx) {
        return varIdx == 0;
    }

    private boolean isTo(int varIdx) {
        return varIdx == 1;
    }

    private boolean isChild(int varIdx) {
        return varIdx >= 2;
    }

    private int getChildVarIndex(int varIdx) {
        return varIdx - 2;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return SetVarEvent.ADDKER_MASK + SetVarEvent.REMENV_MASK + SetVarEvent.INSTSET_MASK;
    }

    @Override
    public void awake() throws ContradictionException {
        DisposableIntIterator it = take.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();
                if (x < 0 || x >= children.length) {
                    take.remFromEnveloppe(x, this, false);
                }
            }
        } finally {
            it.dispose();
        }
        propagate();
    }

    @Override
    public void awakeOnEnvRemovals(int idx, DisposableIntIterator deltaDomain) throws ContradictionException {
        if (isTake(idx)) {
            pruneTo();
            pickTake();
        } else if (isTo(idx)) {
            while (deltaDomain.hasNext()) {
                int val = deltaDomain.next();
                // prune children
                DisposableIntIterator it = take.getDomain().getKernelIterator();
                try {
                    while (it.hasNext()) {
                        children[it.next()].remFromEnveloppe(val, this, false);
                    }
                } finally {
                    it.dispose();
                }
            }
        } else {
            assert isChild(idx);
            while (deltaDomain.hasNext()) {
                int val = deltaDomain.next();
                if (to.isInDomainEnveloppe(val)) {
                    // prune to
                    // pick take
                    DisposableIntIterator it = take.getDomain().getEnveloppeIterator();
                    int child = -1;
                    try {
                        while (it.hasNext()) {
                            int y = it.next();
                            if (children[y].isInDomainEnveloppe(val)) {
                                if (child != -1 || !to.isInDomainKernel(val)) {
                                    // Found a second child.
                                    // Or found a child but can't pick take.
                                    return;
                                }
                                child = y;
                            }
                        }
                    } finally {
                        it.dispose();
                    }
                    if (child == -1) {
                        // prune to
                        to.remFromEnveloppe(val, this, false);
                    } else {
                        // pick take
                        take.addToKernel(child, this, false);
                        Util.kerSubsetOf(this, children[child], to);
                        children[child].addToKernel(val, this, false);
                    }
                }
            }
        }
    }

    @Override
    public void awakeOnkerAdditions(int idx, DisposableIntIterator deltaDomain) throws ContradictionException {
        if (isTake(idx)) {
            while (deltaDomain.hasNext()) {
                int val = deltaDomain.next();
                pruneChildren(val);
                pickTo(val);
            }
        } else if (isTo(idx)) {
            while (deltaDomain.hasNext()) {
                pickTake(deltaDomain.next());
            }
        } else {
            assert isChild(idx);
            // pick to
            if (take.isInDomainKernel(getChildVarIndex(idx))) {
                while (deltaDomain.hasNext()) {
                    to.addToKernel(deltaDomain.next(), this, false);
                }
            }
        }
    }

    @Override
    public void awakeOnInst(int varIdx) throws ContradictionException {
        if (!isTo(varIdx)) {
            assert isTake(varIdx) || isChild(varIdx);
            checkEntailed();
        }
    }

    @Override
    public void propagate() throws ContradictionException {
        // forall{i in ker(take)} children[i] subsetof to
        pruneChildren();
        // env(to) subsetof union{i in env(take)} env(children[i])
        pruneTo();
        pickTake();
        // forall{i in ker(take)} ker(to) subsetof ker(children[i])
        pickTo();
        checkEntailed();
    }

//    private void pruneDisjointChildren() throws ContradictionException {
//        for (int i = 0; i < children.length; i++) {
//            DisposableIntIterator it = children[i].getDomain().getKernelIterator();
//            try {
//                while (it.hasNext()) {
//                    int x = it.next();
//                    for (int j = 0; j < children.length; j++) {
//                        if (i != j) {
//                            children[j].remFromEnveloppe(x, this, false);
//                        }
//                    }
//                }
//            } finally {
//                it.dispose();
//            }
//        }
//    }
    private void pruneChildren() throws ContradictionException {
        DisposableIntIterator it = take.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                pruneChildren(it.next());
            }
        } finally {
            it.dispose();
        }
    }

    private void pruneChildren(int takeVal) throws ContradictionException {
        assert take.isInDomainKernel(takeVal);
        Util.envSubsetOf(this, children[takeVal], to);
    }

    private void pickTake() throws ContradictionException {
        DisposableIntIterator it = to.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                pickTake(it.next());
            }
        } finally {
            it.dispose();
        }
    }

    private void pickTake(int toVal) throws ContradictionException {
        assert to.isInDomainKernel(toVal);

        DisposableIntIterator it = take.getDomain().getEnveloppeIterator();

        // Even though the children are disjoint, there env may not be.
        int child = -1;
        try {
            while (it.hasNext()) {
                int y = it.next();
                if (children[y].isInDomainEnveloppe(toVal)) {
                    if (child != -1) {
                        // Found a second child.
                        return;
                    }
                    child = y;
                }
            }
        } finally {
            it.dispose();
        }
        if (child != -1) {
            take.addToKernel(child, this, false);
            Util.kerSubsetOf(this, children[child], to);
            children[child].addToKernel(toVal, this, false);
        }
    }

    private void pruneTo() throws ContradictionException {
        TIntHashSet keep = new TIntHashSet();

        DisposableIntIterator it = take.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                Util.enumerateEnv(keep, children[it.next()]);
            }
        } finally {
            it.dispose();
        }

        Util.pruneEnv(this, keep, to);
    }

    /**
     * Don't need to prune/pick take/children/take cards since pick to
     * is very good at enforcing cardinality.
     */
    private void pickTo() throws ContradictionException {
        DisposableIntIterator it = take.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                pickTo(it.next());
            }
        } finally {
            it.dispose();
        }
    }

    private void pickTo(int takeVal) throws ContradictionException {
        assert take.isInDomainKernel(takeVal);
        Util.kerSubsetOf(this, children[takeVal], to);
    }

    private void checkEntailed() throws ContradictionException {
        if (take.isInstantiated() && allChildrenInstantiated()) {
            TIntHashSet $to = new TIntHashSet();
            DisposableIntIterator it = take.getDomain().getKernelIterator();
            try {
                while (it.hasNext()) {
                    Util.enumerateEnv($to, children[it.next()]);
                }
            } finally {
                it.dispose();
            }
            to.instantiate($to.toArray(), this, false);
            setEntailed();
        }
    }

    private boolean allChildrenInstantiated() {
        DisposableIntIterator it = take.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                if (!children[it.next()].isInstantiated()) {
                    return false;
                }
            }
        } finally {
            it.dispose();
        }
        return true;
    }

    @Override
    public boolean isConsistent() {
        return (isEntailed() == Boolean.TRUE);
    }

    @Override
    public boolean isSatisfied() {
        return true;
    }
}
