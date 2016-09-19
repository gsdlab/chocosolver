package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 *
 * @author jimmy
 */
public class PropContinuousUnion extends Propagator<Variable> {

    private final SetVar[] sets;
    private final ISetDeltaMonitor[] setsD;
    private final IntVar totalCard;
    private final int[] supports;

    public PropContinuousUnion(SetVar[] sets, IntVar totalCard) {
        super(buildArray(sets, totalCard), PropagatorPriority.BINARY, true);
        this.sets = sets;
        this.setsD = PropUtil.monitorDeltas(sets, this);
        this.totalCard = totalCard;
        this.supports = new int[totalCard.getUB()];
    }

    private static Variable[] buildArray(SetVar[] sets, IntVar totalCard) {
        Variable[] variables = new Variable[sets.length + 1];
        System.arraycopy(sets, 0, variables, 0, sets.length);
        variables[sets.length] = totalCard;
        return variables;
    }

    boolean isSetVar(int idx) {
        return idx < sets.length;
    }

    int getSetVarIndex(int idx) {
        return idx;
    }

    boolean isTotalCardVar(int idx) {
        return idx == sets.length;
    }

//TODO: Sept16
//    @Override
//    public int getPropagationConditions(int vIdx) {
//        if (isSetVar(vIdx)) {
//            return SetEventType.all();
//        }
//        assert isTotalCardVar(vIdx);
//        return IntEventType.all();
//    }
    private boolean support(int value) {
        if (value >= supports.length) {
            return false;
        }
        int support = supports[value];
        if (sets[support].getUB().contains(value)) {
            return true;
        }
        for (int i = 0; i < sets.length; i++) {
            if (sets[i].getUB().contains(value)) {
                supports[value] = i;
                return true;
            }
        }
        return false;
    }

    private void findMate(int unionKer) throws ContradictionException {
        int mate = -1;
        for (int j = 0; j < sets.length; j++) {
            if (sets[j].getUB().contains(unionKer)) {
                // Found a second mate or in kernel.
                if (mate != -1 || sets[j].getLB().contains(unionKer)) {
                    return;
                }
                mate = j;
            }
        }
        if (mate == -1) {
            // No mates.
            fails();
        } else if (mate != -2) {
            // One mate.
            sets[mate].force(unionKer, this);
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int maxEnv = Integer.MIN_VALUE;
        int maxKer = Integer.MIN_VALUE;
        for (SetVar set : sets) {
            if (!set.getUB().isEmpty()) {
                maxEnv = Math.max(maxEnv, set.getUB().max());
            }
            if (!set.getLB().isEmpty()) {
                maxKer = Math.max(maxKer, set.getLB().max());
            }
        }
        totalCard.updateUpperBound(maxEnv == Integer.MIN_VALUE ? 0 : maxEnv + 1, this);
        totalCard.updateLowerBound(maxKer == Integer.MIN_VALUE ? 0 : maxKer + 1, this);

        int ub = totalCard.getUB();
        for (int i = 0; i < ub; i++) {
            if (!support(i)) {
                totalCard.updateUpperBound(i, this);
                break;
            }
        }
        for (SetVar set : sets) {
            ISetIterator iter = set.getUB().iterator();
            while (iter.hasNext()) {
                int i = iter.nextInt();
                if (i < 0 || i >= totalCard.getUB()) {
                    set.remove(i, this);
                }
            }
        }
        int lb = totalCard.getLB();
        for (int i = 0; i < lb; i++) {
            findMate(i);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            int id = getSetVarIndex(idxVarInProp);
            setsD[id].freeze();
            setsD[id].forEach(this::pruneUnionOnSetEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            setsD[id].forEach(this::pickUnionOnSetKer, SetEventType.ADD_TO_KER);
            setsD[id].unfreeze();
        } else {
            assert isTotalCardVar(idxVarInProp);

            int lb = totalCard.getLB();
            for (int i = 0; i < lb; i++) {
                findMate(i);
            }
            for (SetVar set : sets) {
                ISetIterator iter = set.getUB().iterator();
                while (iter.hasNext()) {
                    int i = iter.nextInt();
                    if (i < 0 || i >= totalCard.getUB()) {
                        set.remove(i, this);
                    }
                }
            }
            int ub = totalCard.getUB();
            for (int i = 0; i < ub; i++) {
                if (!support(i)) {
                    totalCard.updateUpperBound(i, this);
                    break;
                }
            }
        }
        propagate(mask);
    }

    private void pruneUnionOnSetEnv(int env) throws ContradictionException {
        int maxEnv = Integer.MIN_VALUE;
        for (SetVar set : sets) {
            if (!set.getUB().isEmpty()) {
                maxEnv = Math.max(maxEnv, set.getUB().max());
            }
        }
        totalCard.updateUpperBound(maxEnv == Integer.MIN_VALUE ? 0 : maxEnv + 1, this);
        int lb = totalCard.getLB();
        if (env < lb) {
            findMate(env);
        }
        if (!support(env)) {
            totalCard.updateUpperBound(env, this);
        }
    }

    private void pickUnionOnSetKer(int ker) throws ContradictionException {
        int f = totalCard.getLB();
        if (totalCard.updateLowerBound(ker + 1, this)) {
            int t = totalCard.getLB();
            for (int i = f; i < t; i++) {
                findMate(i);
            }
        }
    }

    @Override
    public ESat isEntailed() {
        for (SetVar set : sets) {
            if (!set.getLB().isEmpty()) {
                if (set.getLB().min() < 0 || set.getLB().max() >= totalCard.getUB()) {
                    return ESat.FALSE;
                }
            }
        }
        if (!totalCard.isInstantiated()) {
            return ESat.UNDEFINED;
        }
        for (SetVar set : sets) {
            if (!set.getUB().isEmpty()) {
                if (set.getUB().min() < 0 || set.getUB().max() >= totalCard.getUB()) {
                    return ESat.UNDEFINED;
                }
            }
        }
        int tc = totalCard.getValue();

        boolean allRealized = true;
        for (int i = 0; i < tc; i++) {
            boolean realized = false;
            boolean support = false;
            for (SetVar set : sets) {
                if (set.getLB().contains(i)) {
                    realized = true;
                    support = true;
                    break;
                } else if (set.getUB().contains(i)) {
                    support = true;
                }
            }
            allRealized &= realized;
            if (!support) {
                return ESat.FALSE;
            }
        }
        return allRealized ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "union(" + Arrays.toString(sets) + ") = {0, 1, ..., " + totalCard + " - 1}";
    }
}
