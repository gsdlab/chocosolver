package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISet;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 *
 * @author jimmy
 */
public class PropContinuous extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar card;

    public PropContinuous(SetVar set, IntVar card) {
        super(new Variable[]{set, card}, PropagatorPriority.QUADRATIC, false);
        this.set = set;
        this.card = card;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isCardVar(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        assert isCardVar(vIdx);
        return IntEventType.all();
    }

    private static boolean containsRange(ISet set, int low, int high) {
        for (int i = low; i <= high; i++) {
            if (!set.contains(low)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int kerMax = 0;
        int kerMin = 0;
        if (!set.getLB().isEmpty()) {
            ISetIterator setKer = set.getLB().iterator();
            assert setKer.hasNext();
            int cur = setKer.nextInt();
            kerMin = cur;
            while (setKer.hasNext()) {
                int next = setKer.nextInt();
                for (int j = cur + 1; j < next; j++) {
                    set.force(j, this);
                }
                cur = next;
            }
            kerMax = cur;
        }
        if (!set.getUB().isEmpty()) {
            int[] regionBounds = new int[2 * set.getUB().size()];
            int regions = iterateRange(set.getUB().iterator(), regionBounds);
            int survivingRegions = regions;
            int maxSize = 0;

            for (int region = 0; region < regions; region++) {
                int low = regionBounds[2 * region];
                int high = regionBounds[2 * region + 1];

                assert !set.getUB().contains(low - 1);
                assert containsRange(set.getUB(), low, high);
                assert !set.getUB().contains(high + 1);

                int size = high - low + 1;
                if (card.previousValue(size + 1) <= 0
                        || (!set.getLB().isEmpty() && (kerMin < low || kerMax > high))) {
                    for (int i = low; i <= high; i++) {
                        set.remove(i, this);
                    }
                    survivingRegions--;
                } else {
                    maxSize = Math.max(maxSize, size);
                }
            }

            card.updateUpperBound(maxSize, this);

            final int cardLb = card.getLB();
            final int cardUb = card.getUB();

            if (survivingRegions == 1) {
                int low = set.getUB().min();
                int high = set.getUB().max();
                int start = high - cardLb + 1;
                if (!set.getLB().isEmpty() && kerMin < start) {
                    start = kerMin;
                }
                int end = low + cardLb - 1;
                if (!set.getLB().isEmpty() && kerMax > end) {
                    end = kerMax;
                }
                for (int i = start; i <= end; i++) {
                    set.force(i, this);
                }
                if (!set.getLB().isEmpty()) {
                    for (int i = low; i <= end - cardUb; i++) {
                        boolean changed = set.remove(i, this);
                        assert changed;
                    }
                    for (int i = high; i >= start + cardUb; i--) {
                        boolean changed = set.remove(i, this);
                        assert changed;
                    }
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (!set.getLB().isEmpty()) {
            int min = set.getLB().min();
            int max = set.getLB().max();
            for (int i = min + 1; i < max; i++) {
                if (!set.getUB().contains(i)) {
                    return ESat.FALSE;
                }
            }
            if (max - min >= card.getUB()) {
                return ESat.FALSE;
            }
        }

        if (!set.getUB().isEmpty()) {
            int ker = set.getLB().isEmpty() ? 0 : set.getLB().min();

            int[] regionBounds = new int[2 * set.getUB().size()];
            int regions = iterateRange(set.getUB().iterator(), regionBounds);
            int maxRegionSize = 0;
            for (int region = 0; region < regions; region++) {
                int low = regionBounds[2 * region];
                int high = regionBounds[2 * region + 1];
                int size = high - low + 1;
                if (set.getLB().isEmpty() || (low <= ker && ker <= high)) {
                    maxRegionSize = Math.max(maxRegionSize, size);
                }
            }

            if (card.getLB() > maxRegionSize) {
                return ESat.FALSE;
            }
            if (card.getUB() <= 1 && maxRegionSize == 1) {
                return ESat.TRUE;
            }
        } else {
            if (!card.contains(0)) {
                return ESat.FALSE;
            }
            return card.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
        }

        return set.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    private static int iterateRange(ISetIterator iter, int[] out) {
        assert iter.hasNext();
        int prev = iter.nextInt();
        int size = 1;
        int regions = 0;
        while (iter.hasNext()) {
            int next = iter.nextInt();
            if (next != prev + 1) {
                out[2 * regions] = prev - size + 1;
                out[2 * regions + 1] = prev;
                regions++;
                size = 0;
            }
            prev = next;
            size++;
        }
        out[2 * regions] = prev - size + 1;
        out[2 * regions + 1] = prev;
        regions++;
        return regions;
    }

    @Override
    public String toString() {
        return "continuous(" + set + ", " + card + ")";
    }
}
