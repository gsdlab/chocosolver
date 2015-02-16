package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIntIterator;
import gnu.trove.map.hash.TIntIntHashMap;
import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropJoinFunctionCard extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar take;
    private final IntVar takeCard;
    private final IntVar[] refs;
    private final IntVar toCard;
    private final Integer globalCardinality;

    public PropJoinFunctionCard(SetVar take, IntVar takeCard, IntVar[] refs, IntVar toCard, Integer globalCardinality) {
        super(buildArray(take, takeCard, toCard, refs), PropagatorPriority.LINEAR, false);
        this.take = take;
        this.takeCard = takeCard;
        this.refs = refs;
        this.toCard = toCard;
        this.globalCardinality = globalCardinality;
    }

    private static Variable[] buildArray(SetVar take, IntVar takeCard, IntVar toCard, IntVar[] refs) {
        Variable[] array = new Variable[refs.length + 3];
        array[0] = take;
        array[1] = takeCard;
        array[2] = toCard;
        System.arraycopy(refs, 0, array, 3, refs.length);
        return array;
    }

    private boolean isTakeVar(int idx) {
        return idx == 0;
    }

    private boolean isTakeCardVar(int idx) {
        return idx == 1;
    }

    private boolean isToCardVar(int idx) {
        return idx == 2;
    }

    private boolean isRefVar(int idx) {
        return idx >= 3;
    }

    private int getRefVarIndex(int idx) {
        assert isRefVar(idx);
        return idx - 3;
    }

    public boolean hasGlobalCardinality() {
        return globalCardinality != null;
    }

    public int getGlobalCardinality() {
        assert hasGlobalCardinality();
        return globalCardinality.intValue();
    }

    @Override
    public boolean advise(int idxVarInProp, int mask) {
        if (isRefVar(idxVarInProp)) {
            return take.envelopeContains(getRefVarIndex(idxVarInProp));
        }
        return super.advise(idxVarInProp, mask);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isTakeVar(vIdx)) {
            return SetEventType.all();
        }
        if (isRefVar(vIdx)) {
            return IntEventType.instantiation();
        }
        assert isTakeCardVar(vIdx) || isToCardVar(vIdx);
        return IntEventType.boundAndInst();
    }

    private int countAdditionalSameRefsAllowed(TIntIntHashMap map) {
        assert hasGlobalCardinality();
        int gc = getGlobalCardinality();

        int allowed = 0;

        if (gc != 1) {
            TIntIntIterator iter = map.iterator();
            for (int i = map.size(); i-- > 0;) {
                iter.advance();
                assert iter.value() > 0;
                assert iter.value() <= gc;
                allowed += gc - iter.value();
            }
            assert !iter.hasNext();
        }

        return allowed;
    }

    private TIntIntHashMap countRefs() {
        TIntIntHashMap map = new TIntIntHashMap(take.getKernelSize());
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            IntVar ref = refs[i];
            if (ref.isInstantiated()) {
                map.adjustOrPutValue(ref.getValue(), 1, 1);
            }
        }
        return map;
    }

    private TIntIntHashMap constrainGlobalCardinality() throws ContradictionException {
        assert hasGlobalCardinality();
        int[] ker = PropUtil.iterateKer(take);
        TIntIntHashMap map = new TIntIntHashMap(ker.length);
        for (int i = 0; i < ker.length; i++) {
            constrainGlobalCardinality(ker, i, i, map);
        }
        return map;
    }

    private void constrainGlobalCardinality(int[] ker, int index, int explored, TIntIntHashMap map) throws ContradictionException {
        assert hasGlobalCardinality();
        assert index <= explored;
        assert explored < ker.length;

        IntVar a = refs[ker[index]];
        if (a.isInstantiated()) {
            int value = a.getValue();
            int count = map.adjustOrPutValue(value, 1, 1);
            int gc = getGlobalCardinality();

            if (count == gc) {
                for (int j = 0; j < explored; j++) {
                    IntVar b = refs[ker[j]];
                    if (!b.isInstantiatedTo(value) && b.removeValue(value, aCause)) {
                        constrainGlobalCardinality(ker, j, explored, map);
                    }
                }
                for (int j = explored + 1; j < ker.length; j++) {
                    refs[ker[j]].removeValue(value, aCause);
                }
            } else if (count > gc) {
                contradiction(a, "Above global cardinality");
            }
        }
    }

    private static int divRoundUp(int a, int b) {
        assert a >= 0;
        assert b > 0;

        return (a + b - 1) / b;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        takeCard.updateLowerBound(take.getKernelSize(), aCause);
        takeCard.updateUpperBound(take.getEnvelopeSize(), aCause);
        boolean changed;
        do {
            changed = false;
            TIntIntHashMap map = hasGlobalCardinality() ? constrainGlobalCardinality() : countRefs();
            int instCard = map.size();
            int kerSize = take.getKernelSize();

            int minUninstantiated;
            int maxUninstantiated;
            int minCard;
            int maxCard;
            boolean cardChanged;
            do {
                cardChanged = false;
                int kerUninstantiated = 0;
                for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
                    if (!refs[i].isInstantiated()) {
                        if (take.kernelContains(i)) {
                            kerUninstantiated++;
                        }
                    }
                }
                assert takeCard.getLB() >= kerSize;
                minUninstantiated = takeCard.getLB() - kerSize + kerUninstantiated;
                maxUninstantiated = takeCard.getUB() - kerSize + kerUninstantiated;
                minCard = instCard
                        + (hasGlobalCardinality()
                                ? divRoundUp(Math.max(0, minUninstantiated - countAdditionalSameRefsAllowed(map)), getGlobalCardinality())
                                : 0);
                maxCard = instCard + maxUninstantiated;

                toCard.updateLowerBound(minCard, aCause);
                toCard.updateUpperBound(maxCard, aCause);

                if (hasGlobalCardinality()) {
                    cardChanged |= takeCard.updateUpperBound(toCard.getUB() * getGlobalCardinality(), aCause);
                }
                cardChanged |= takeCard.updateLowerBound(toCard.getLB(), aCause);
            } while (cardChanged);

            if (maxUninstantiated != 0) {
                if (instCard == toCard.getUB()) {
                    // The rest must be duplicates.
                    for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
                        IntVar ref = refs[i];
                        assert !ref.isInstantiated() || map.contains(ref.getValue());
                        if (!ref.isInstantiated()) {
                            changed |= PropUtil.domSubsetSet(ref, map.keySet(), aCause) && ref.isInstantiated();
                        }
                    }
                }
                if (maxCard == toCard.getLB()) {
                    // No more duplicate values.
                    for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
                        IntVar ref = refs[i];
                        if (!ref.isInstantiated()) {
                            TIntIntIterator iter = map.iterator();
                            for (int j = map.size(); j-- > 0;) {
                                iter.advance();
                                changed |= ref.removeValue(iter.key(), aCause) && ref.isInstantiated();
                            }
                            assert !iter.hasNext();
                        }
                    }
                }
            }
        } while (changed);
    }

    @Override
    public ESat isEntailed() {
        TIntIntHashMap map = new TIntIntHashMap();
        int gc = hasGlobalCardinality() ? getGlobalCardinality() : Integer.MAX_VALUE;
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            if (i < 0 || i >= refs.length) {
                return ESat.FALSE;
            }
            IntVar ref = refs[i];
            if (ref.isInstantiated()) {
                if (map.adjustOrPutValue(ref.getValue(), 1, 1) > gc) {
                    return ESat.FALSE;
                }
            }
        }

        boolean completelyInstantiated = take.isInstantiated() && takeCard.isInstantiated() && toCard.isInstantiated();
        int instCard = map.size();
        int minUninstantiated = Math.max(0, takeCard.getLB() - take.getKernelSize());
        int maxUninstantiated = Math.max(0, takeCard.getUB() - take.getKernelSize());
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (i >= 0 && i < refs.length && !refs[i].isInstantiated()) {
                completelyInstantiated = false;
                if (take.kernelContains(i)) {
                    minUninstantiated++;
                }
                maxUninstantiated++;
            }
        }
        int minCard = instCard
                + (hasGlobalCardinality()
                        ? divRoundUp(Math.max(0, minUninstantiated - countAdditionalSameRefsAllowed(map)), getGlobalCardinality())
                        : 0);
        int maxCard = instCard + maxUninstantiated;

        if (toCard.getUB() < minCard) {
            return ESat.FALSE;
        }
        if (toCard.getLB() > maxCard) {
            return ESat.FALSE;
        }

        return completelyInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "joinFunctionCard(" + take + ", " + takeCard + ", " + Arrays.toString(refs) + ", " + toCard + ", " + globalCardinality + ")";
    }
}
