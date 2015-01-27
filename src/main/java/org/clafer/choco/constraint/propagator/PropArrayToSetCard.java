package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIntIterator;
import gnu.trove.map.hash.TIntIntHashMap;
import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropArrayToSetCard extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final IntVar[] as;
    private final IntVar sCard;
    private final Integer globalCardinality;

    public PropArrayToSetCard(IntVar[] as, IntVar sCard, Integer globalCardinality) {
        super(buildArray(sCard, as), PropagatorPriority.LINEAR, false);
        if (as.length == 0) {
            throw new IllegalArgumentException();
        }
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
            return IntEventType.instantiation();
        }
        assert isSCardVar(vIdx);
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
        TIntIntHashMap map = new TIntIntHashMap(as.length);
        for (IntVar a : as) {
            if (a.isInstantiated()) {
                map.adjustOrPutValue(a.getValue(), 1, 1);
            }
        }
        return map;
    }

    private TIntIntHashMap constrainGlobalCardinality() throws ContradictionException {
        assert hasGlobalCardinality();
        TIntIntHashMap map = new TIntIntHashMap(as.length);
        for (int i = 0; i < as.length; i++) {
            constrainGlobalCardinality(i, i, map);
        }
        assert map.size() == countRefs().size();
        return map;
    }

    private void constrainGlobalCardinality(int index, int explored, TIntIntHashMap map) throws ContradictionException {
        assert hasGlobalCardinality();
        assert index <= explored;
        assert explored < as.length;

        IntVar a = as[index];
        if (a.isInstantiated()) {
            int value = a.getValue();
            int count = map.adjustOrPutValue(value, 1, 1);
            int gc = getGlobalCardinality();

            if (count == gc) {
                for (int j = 0; j < explored; j++) {
                    IntVar b = as[j];
                    if (!b.isInstantiatedTo(value) && b.removeValue(value, aCause)) {
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
            TIntIntHashMap map = hasGlobalCardinality() ? constrainGlobalCardinality() : countRefs();
            int instCard = map.size();
            int uninstantiated = 0;
            for (IntVar a : as) {
                if (!a.isInstantiated()) {
                    uninstantiated++;
                }
            }
            int minCard = Math.max(1, instCard
                    + (hasGlobalCardinality()
                    ? divRoundUp(Math.max(0, uninstantiated - countAdditionalSameRefsAllowed(map)), getGlobalCardinality())
                    : 0));
            int maxCard = instCard + uninstantiated;

            sCard.updateLowerBound(minCard, aCause);
            sCard.updateUpperBound(maxCard, aCause);

            if (uninstantiated != 0) {
                if (instCard == sCard.getUB()) {
                    // The rest must be duplicates.
                    for (IntVar a : as) {
                        assert !a.isInstantiated() || map.contains(a.getValue());
                        if (!a.isInstantiated()) {
                            changed |= PropUtil.domSubsetSet(a, map.keySet(), aCause) && a.isInstantiated();
                        }
                    }
                }
                if (maxCard == sCard.getLB()) {
                    // No more duplicate values.
                    for (IntVar a : as) {
                        if (!a.isInstantiated()) {
                            TIntIntIterator iter = map.iterator();
                            for (int i = map.size(); i-- > 0;) {
                                iter.advance();
                                changed |= a.removeValue(iter.key(), aCause) && a.isInstantiated();
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
        int uninstantiated = 0;
        for (IntVar a : as) {
            if (a.isInstantiated()) {
                if (map.adjustOrPutValue(a.getValue(), 1, 1) > gc) {
                    return ESat.FALSE;
                }
            } else {
                uninstantiated++;
            }
        }

        int instCard = map.size();
        int minCard = instCard
                + (hasGlobalCardinality()
                ? divRoundUp(Math.max(0, uninstantiated - countAdditionalSameRefsAllowed(map)), getGlobalCardinality())
                : 0);
        int maxCard = instCard + uninstantiated;

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
