package org.clafer.constraint;

import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractLargeSetSConstraint;
import choco.kernel.solver.variables.integer.IntDomain;
import choco.kernel.solver.variables.set.SetVar;
import gnu.trove.TIntHashSet;
import org.clafer.Util;

/**
 * Assumption: Children are disjoint!
 * 
 * @author jimmy
 */
public class Join extends AbstractLargeSetSConstraint {

    private final SetVar take;
    private final SetVar[] children;
    private final SetVar to;

    public Join(SetVar take, SetVar[] children, SetVar to) {
        super(buildArray(take, children, to));
        this.take = take;
        this.children = children;
        this.to = to;
    }

    private static SetVar[] buildArray(SetVar take, SetVar[] children, SetVar to) {
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

    private int getChildVarIndex(int varIdx) {
        return varIdx - 2;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return SetVarEvent.ADDKER_MASK + SetVarEvent.REMENV_MASK;
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

//    @Override
//    public void awakeOnKer(int varIdx, int x) throws ContradictionException {
//        if (isTake(varIdx)) {
//            // pick to
//            Util.kerSubsetOf(this, children[x], to);
//            Util.envSubsetOf(this, children[x], to);
//        } else if (isTo(varIdx)) {
//            instanciateIfLastOccurence(x);
//        } else {
//            propagate();
//            int c = getChildVarIndex(varIdx);
//            // prune disjoint children
//            for (int i = 0; i < children.length; i++) {
//                if (i != c) {
//                    children[i].remFromEnveloppe(x, this, false);
//                }
//            }
//            // pick to
//            if (take.isInDomainKernel(c)) {
//                to.addToKernel(x, this, false);
//            }
//        }
//    }
//    @Override
//    public void awakeOnEnv(int varIdx, int x) throws ContradictionException {
//        if (isTake(varIdx)) {
//            pruneTo();
//        } else if (isTo(varIdx)) {
//            // pick to
//            DisposableIntIterator it = take.getDomain().getKernelIterator();
//            try {
//                while (it.hasNext()) {
//                    int y = it.next();
//
//                    children[y].remFromEnveloppe(x, this, false);
//                }
//            } finally {
//                it.dispose();
//            }
//        } else {
//            int c = getChildVarIndex(varIdx);
//            if (take.isInDomainKernel(c)) {
//                if (to.isInDomainKernel(x)) {
//                    instanciateIfLastOccurence(x);
//                }
//                // pick to
//                removeIfNoOccurence(x);
//            }
//        }
//    }
//
//    private void removeIfNoOccurence(int x) throws ContradictionException {
//        DisposableIntIterator it = take.getDomain().getKernelIterator();
//        try {
//            while (it.hasNext()) {
//                int y = it.next();
//
//                if (children[y].isInDomainEnveloppe(x)) {
//                    return;
//                }
//            }
//        } finally {
//            it.dispose();
//        }
//        to.remFromEnveloppe(x, this, false);
//    }
//    @Override
//    public void awakeOnInst(int varIdx) throws ContradictionException {
//        // Do nothing
//    }
    @Override
    public void propagate() throws ContradictionException {
        pruneDisjointChildren();
        pruneChildren();
        instanciateLastOccurences();
        pruneTo();
        pickTo();
    }

    private void pruneChildren() throws ContradictionException {
        DisposableIntIterator it = take.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                Util.envSubsetOf(this, children[it.next()], to);
            }
        } finally {
            it.dispose();
        }
    }

    private void pruneDisjointChildren() throws ContradictionException {
        for (int i = 0; i < children.length; i++) {
            DisposableIntIterator it = children[i].getDomain().getKernelIterator();
            try {
                while (it.hasNext()) {
                    int x = it.next();
                    for (int j = 0; j < children.length; j++) {
                        if (i != j) {
                            children[j].remFromEnveloppe(x, this, false);
                        }
                    }
                }
            } finally {
                it.dispose();
            }
        }
    }

    private void instanciateLastOccurences() throws ContradictionException {
        DisposableIntIterator it = to.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                instanciateIfLastOccurence(it.next());
            }
        } finally {
            it.dispose();
        }
    }

    private void instanciateIfLastOccurence(int x) throws ContradictionException {
        assert to.isInDomainKernel(x);

        DisposableIntIterator it = take.getDomain().getEnveloppeIterator();

        // Even though the children are disjoint, there env may not be.
        int child = -1;
        try {
            while (it.hasNext()) {
                int y = it.next();
                if (children[y].isInDomainEnveloppe(x)) {
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
            children[child].addToKernel(x, this, false);
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
                Util.kerSubsetOf(this, children[it.next()], to);
            }
        } finally {
            it.dispose();
        }

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
