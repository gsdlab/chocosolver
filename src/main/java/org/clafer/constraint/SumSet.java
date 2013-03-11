package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.cp.solver.variables.set.BitSetEnumeratedDomain;
import choco.cp.solver.variables.set.SetVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractBinSetIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetVar;
import org.clafer.Util;
import org.clafer.collection.FixedIntQueue;
import org.clafer.collection.IntIterator;

/**
 * Implementation requires hints from the set's cardinality.
 * This is doable because integers in Clafer mainly come from dereferencing.
 * 
 * eg.
 *   Age.ref
 * 
 * Can determine the upper bound on the expression. Requires help from JoinRef
 * to keep an upper bound on the cardinalities. This upper bound is generally small
 * (it's bounded by the scope of Age in this example) and optimizations of this
 * constraint assume this to be the case.
 * 
 * Take advantage of the fact that set.getCard().getSup() may be smaller than
 * set.getEnveloppeDomainSize().
 * 
 * @author jimmy
 */
public class SumSet extends AbstractBinSetIntSConstraint {

    public SumSet(SetVar set, IntDomainVar sum) {
        super(sum, set);
    }

    @Override
    public int getFilteredEventMask(int idx) {
        if (idx == 0) {
            return IntVarEvent.REMVAL_MASK + IntVarEvent.INSTINT_MASK;
        }
        return SetVarEvent.ADDKER_MASK + SetVarEvent.REMENV_MASK + SetVarEvent.INSTSET_MASK;
    }

    @Override
    public void propagate() throws ContradictionException {
        pruneSet();
        pruneSum();
    }

    private void pruneSet() throws ContradictionException {
        switch (v1.getCard().getSup() - v1.getKernelDomainSize()) {
            case 1:
                pruneSet1Left();
                break;
            case 2:
                pruneSet2Left();
                break;
        }
    }

    private void pruneSet1Left() throws ContradictionException {
        assert v1.getCard().getSup() - v1.getKernelDomainSize() == 1;

        final int[] ker = Util.iterateKer(v1);
        final int s = Util.sum(ker);

        DisposableIntIterator it = v1.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int val = it.next();
                if (!Util.in(val, ker) && !v0.canBeInstantiatedTo(s + val)) {
                    v1.remFromEnveloppe(val, this, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    private void pruneSet2Left() throws ContradictionException {
        assert v1.getCard().getSup() - v1.getKernelDomainSize() == 2;

        // It's also possible to do this pruning if sum isn't instantiated
        // but it's likely too expensive unless |dom(sum)| is small.
        if (v0.isInstantiated()) {
            final int sum = v0.getVal();
            final int[] ker = Util.iterateKer(v1);
            final int s = Util.sum(ker);
            DisposableIntIterator it = v1.getDomain().getEnveloppeIterator();
            try {
                while (it.hasNext()) {
                    int val = it.next();
                    if (!Util.in(val, ker)
                            && sum != s + val
                            && !v1.isInDomainEnveloppe(sum - (s + val))) {
                        v1.remFromEnveloppe(val, this, false);
                    }
                }
            } finally {
                it.dispose();
            }
        }
    }

    private void pruneSum() throws ContradictionException {
        // Cannot use this faster method due to a bug in getPrevValue
        // https://sourceforge.net/projects/choco/forums/forum/335512/topic/6890143
        // pruneMinMaxSumSet();
        pruneMinMaxSumSetGeneric();
    }

    /**
     * Assumes SetDomainImpl! Seems to be the only concrete class of SetDomain
     * so safe for now. Significantly faster so worth the risk. Switch to
     * pruneMinMaxSumSetGeneric if this assumption is proven false.
     * 
     * SetDomainImpl has SetSubDomain of BitSetEnumeratedDomain which has 2
     * important features. It is sorted so it's easy to retrieve the smallest
     * n integers. It also implements getPrevValue and getLastVal so it is
     * possible it iterate from high to low to retrieve the largest n integers.
     */
    private void pruneMinMaxSumSet() throws ContradictionException {
        if (true) {
            throw new Error("TODO: update with card pruning");
        }
        final int minC = Math.min(v1.getCard().getInf(), v1.getEnveloppeDomainSize());
        final int maxC = Math.min(v1.getCard().getSup(), v1.getEnveloppeDomainSize());

        final int count = v1.getKernelDomainSize();

        assert maxC >= count;
        if (maxC < count) {
            fail();
        }
        if (maxC == count) {
            v0.instantiate(Util.sumAndDispose(v1.getDomain().getKernelIterator()), this, false);
            return;
        }

        final int[] ker = Util.iterateKer(v1);
        final int s = Util.sum(ker);

        assert maxC > count;
        assert v1.getEnveloppeDomainSize() > 0;

        int min = s;
        {// Calculate min
            BitSetEnumeratedDomain dom = (BitSetEnumeratedDomain) v1.getDomain().getEnveloppeDomain();
            assert dom.getSize() > 0;
            int smallest = Integer.MIN_VALUE;
            int i = count;
            while (i < minC) {
                smallest = (smallest == Integer.MIN_VALUE) ? dom.getFirstVal() : dom.getNextValue(smallest);

                if (!Util.in(smallest, ker)) {
                    min += smallest;
                    i++;
                }
            }
            while (i < maxC && (smallest == Integer.MIN_VALUE || smallest < dom.getLastVal())) {
                smallest = (smallest == Integer.MIN_VALUE) ? dom.getFirstVal() : dom.getNextValue(smallest);
                if (smallest > 0) {
                    break;
                }
                if (!Util.in(smallest, ker)) {
                    min += smallest;
                    i++;
                }
            }
        }
        v0.updateInf(min, this, false);

        int max = s;
        {// Calculate max
            BitSetEnumeratedDomain dom = (BitSetEnumeratedDomain) v1.getDomain().getEnveloppeDomain();
            assert dom.getSize() > 0;
            int largest = Integer.MIN_VALUE;
            int i = count;
            while (i < minC) {
                largest = (largest == Integer.MIN_VALUE) ? dom.getLastVal() : dom.getPrevValue(largest);

                if (!Util.in(largest, ker)) {
                    max += largest;
                    i++;
                }
            }
            while (i < maxC && (largest == Integer.MIN_VALUE || largest > dom.getFirstVal())) {
                largest = (largest == Integer.MIN_VALUE) ? dom.getLastVal() : dom.getPrevValue(largest);
                if (largest < 0) {
                    break;
                }
                if (!Util.in(largest, ker)) {
                    max += largest;
                    i++;
                }
            }
        }
        v0.updateSup(max, this, false);
    }

    private void pruneMinMaxSumSetGeneric() throws ContradictionException {
        final int minC = Math.min(v1.getCard().getInf(), v1.getEnveloppeDomainSize());
        final int maxC = Math.min(v1.getCard().getSup(), v1.getEnveloppeDomainSize());

        final int count = v1.getKernelDomainSize();

        assert maxC >= count;
        if (maxC < count) {
            fail();
        }
        if (maxC == count) {
            v0.instantiate(Util.sumAndDispose(v1.getDomain().getKernelIterator()), this, false);
            return;
        }

        final int[] ker = Util.iterateKer(v1);
        final int s = Util.sum(ker);

        assert maxC > count;

        FixedIntQueue minQ = FixedIntQueue.smallest(maxC - count);
        FixedIntQueue maxQ = FixedIntQueue.largest(maxC - count);

        DisposableIntIterator it = v1.getDomain().getEnveloppeIterator();

        try {
            while (it.hasNext()) {
                int val = it.next();
                if (!Util.in(val, ker)) {
                    minQ.add(val);
                    maxQ.add(val);
                }
            }
        } finally {
            it.dispose();
        }

        {// Calculate min
            int min = s;
            IntIterator mini = minQ.toIterator();
            int i = count;
            for (; i < minC; i++) {
                min += mini.next();
            }
            for (; i < maxC /*&& mini.hasNext()*/; i++) {
                int val = mini.next();
                if (val > 0) {
//                    System.out.println(min);
//                    DisposableIntIterator itt = v1.getDomain().getEnveloppeIterator();
//                    try {
//                        while (itt.hasNext()) {
//                            int vall = itt.next();
//
//                            if (vall + min > v0.getSup()) {
//                                v1.remFromEnveloppe(vall, this, false);
//                            }
//                        }
//                    } finally {
//                        itt.dispose();
//                    }
                    break;
                }
                min += val;
            }
//            if (mini.hasNext()) {
//                int val = mini.next();
//                if (val > 0) {
//                    System.out.println(min);
//                    DisposableIntIterator itt = v1.getDomain().getEnveloppeIterator();
//                    try {
//                        while (itt.hasNext()) {
//                            int vall = itt.next();
//
//                            if (vall + min > v0.getSup()) {
//                                v1.remFromEnveloppe(vall, this, false);
//                            }
//                        }
//                    } finally {
//                        itt.dispose();
//                    }
//                }
//            }

            v0.updateInf(min, this, false);
        }
        { // Prune card sup
            int min = s;
            IntIterator mini = minQ.toIterator();
            for (int i = count; i < maxC; i++) {
                if (i > count) {
                    int val = mini.next();
                    min += val;
                }
                if (min > v0.getSup()) {
                    v1.getCard().updateSup(i, this, false);
                    break;
                }
            }
        }

        {//Calculate max
            int max = s;
            IntIterator maxi = maxQ.toIterator();
            int i = count;
            for (; i < minC; i++) {
                max += maxi.next();
            }
            for (; i < maxC /*&& maxi.hasNext()*/; i++) {
                int val = maxi.next();
                if (val < 0) {
                    break;
                }
                max += val;
            }
            v0.updateSup(max, this, false);
        }
        { // Prune card inf
            int max = s;
            IntIterator maxi = maxQ.toIterator();
            for (int i = count; i < maxC; i++) {
                if (i > count) {
                    int val = maxi.next();
                    if (val < 0) {
                        // Gone through all the positives and the largest
                        // sum possible is smaller than the domain of sum var.
                        fail();
                    }
                    max += val;
                }
                if (max < v0.getInf()) {
                    v1.getCard().updateInf(i, this, false);
                } else {
                    break;
                }
            }
        }
    }

    @Override
    public boolean isSatisfied() {
        int s = 0;

        DisposableIntIterator it = v1.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                s += it.next();
            }
        } finally {
            it.dispose();
        }

        return v0.getVal() == s;
    }
}
