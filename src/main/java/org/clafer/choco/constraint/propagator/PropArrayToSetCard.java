package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.set.TIntSet;
import java.util.Arrays;
import org.clafer.collection.FixedCapacityIntSet;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.Variable;
import util.ESat;

/**
 * TODO propping on card
 *
 * @author jimmy
 */
public class PropArrayToSetCard extends Propagator<Variable> {

    private final IntVar[] as;
    private final IntVar sCard;
    private final Integer globalCardinality;

    public PropArrayToSetCard(IntVar[] as, IntVar sCard, Integer globalCardinality) {
        super(buildArray(sCard, as), PropagatorPriority.LINEAR, false);
        this.as = as;
        this.sCard = sCard;
        this.globalCardinality = globalCardinality;
    }

    private static Variable[] buildArray(IntVar sCard, IntVar[] as) {
        Variable[] array = new Variable[as.length + 1];
        array[0] = sCard;
        System.arraycopy(as, 0, array, 1, as.length);
        return array;
    }

    private boolean isAVar(int idx) {
        return idx >= 1;
    }

    private int getAVarIndex(int idx) {
        assert isAVar(idx);
        return idx - 1;
    }

    private boolean isSCardVar(int idx) {
        return idx == 0;
    }

    public boolean hasGlobalCardinality() {
        return globalCardinality != null;
    }

    public int getGlobalCardinality() {
        assert hasGlobalCardinality();
        return globalCardinality.intValue();
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isAVar(vIdx)) {
            return EventType.INSTANTIATE.mask;
        }
        assert isSCardVar(vIdx);
        return EventType.BOUND.mask + EventType.INSTANTIATE.mask;
    }

    private TIntSet countRefs() {
        FixedCapacityIntSet set = new FixedCapacityIntSet(as.length);
        for (IntVar a : as) {
            if (a.instantiated()) {
                set.add(a.getValue());
            }
        }
        return set;
    }

    private TIntSet constrainGlobalCardinality() throws ContradictionException {
        assert hasGlobalCardinality();
        TIntIntHashMap map = new TIntIntHashMap(as.length);
        for (int i = 0; i < as.length; i++) {
            constrainGlobalCardinality(i, i, map);
        }
        assert map.size() == countRefs().size();
        return map.keySet();
    }

    private void constrainGlobalCardinality(int index, int explored, TIntIntHashMap map) throws ContradictionException {
        assert hasGlobalCardinality();
        assert index <= explored;
        assert explored < as.length;

        IntVar a = as[index];
        if (a.instantiated()) {
            int value = a.getValue();
            int count = map.adjustOrPutValue(value, 1, 1);
            int gc = getGlobalCardinality();

            if (count == gc) {
                for (int j = 0; j < explored; j++) {
                    IntVar b = as[j];
                    if (!b.instantiatedTo(value) && b.removeValue(value, aCause)) {
                        constrainGlobalCardinality(j, explored, map);
                    }
                }
                for (int j = explored + 1; j < as.length; j++) {
                    as[j].removeValue(value, aCause);
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
        boolean changed;
        do {
            changed = false;
            TIntSet instantiated = hasGlobalCardinality() ? constrainGlobalCardinality() : countRefs();
            int instCard = instantiated.size();
            int uninstantiated = 0;
            for (IntVar a : as) {
                if (!a.instantiated()) {
                    uninstantiated++;
                }
            }
            int minCard = instCard + (hasGlobalCardinality()
                    ? divRoundUp(uninstantiated, getGlobalCardinality())
                    : 0);
            int maxCard = instCard + uninstantiated;

            sCard.updateLowerBound(minCard, aCause);
            sCard.updateUpperBound(maxCard, aCause);

            if (uninstantiated != 0) {
                if (instCard == sCard.getUB()) {
                    // The rest must be duplicates.
                    for (IntVar a : as) {
                        assert !a.instantiated() || instantiated.contains(a.getValue());
                        if (!a.instantiated()) {
                            changed |= PropUtil.domainSubsetOf(a, instantiated, aCause) && a.instantiated();
                        }
                    }
                }
                if (maxCard == sCard.getLB()) {
                    // No more duplicate values.
                    for (IntVar a : as) {
                        if (!a.instantiated()) {
                            TIntIterator iter = instantiated.iterator();
                            while (iter.hasNext()) {
                                changed |= a.removeValue(iter.next(), aCause) && a.instantiated();
                            }
                        }
                    }
                }
            }
        } while (changed);
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        TIntIntHashMap map = new TIntIntHashMap();
        int gc = hasGlobalCardinality() ? getGlobalCardinality() : Integer.MAX_VALUE;
        int uninstantiated = 0;
        for (IntVar a : as) {
            if (a.instantiated()) {
                if (map.adjustOrPutValue(a.getValue(), 1, 1) > gc) {
                    return ESat.FALSE;
                }
            } else {
                uninstantiated++;
            }
        }

        int minCard = map.size();
        int maxCard = map.size() + uninstantiated;

        if (sCard.getUB() < minCard) {
            return ESat.FALSE;
        }
        if (sCard.getLB() > maxCard) {
            return ESat.FALSE;
        }

        return uninstantiated == 0 ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "arrayToSetCard(" + Arrays.toString(as) + ", " + sCard + ", " + globalCardinality + ")";
    }
}
