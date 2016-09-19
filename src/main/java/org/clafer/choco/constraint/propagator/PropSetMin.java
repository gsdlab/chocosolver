package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 * The smallest element in the set. Does nothing if the set is empty.
 *
 * @author jimmy
 */
public class PropSetMin extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar setCard;
    private final IntVar min;
    private final int defaultValue;

    public PropSetMin(SetVar set, IntVar setCard, IntVar min, int defaultValue) {
        super(new Variable[]{set, setCard, min}, PropagatorPriority.LINEAR, false);
        this.set = set;
        this.setCard = setCard;
        this.min = min;
        this.defaultValue = defaultValue;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (!set.getLB().isEmpty()) {
            min.updateUpperBound(set.getLB().min(), this);
        }

        int lb = min.getLB();
        while (lb <= min.getUB() && !set.getUB().contains(lb)) {
            lb = min.nextValue(lb);
        }
        ISetIterator iter = set.getUB().iterator();
        int i;
        while (iter.hasNext() && (i = iter.nextInt()) < lb) {
            set.remove(i, this);
        }
        if (!set.getLB().isEmpty() && set.getLB().size() + 1 == setCard.getUB()) {
            iter.reset();
            int kerMin = set.getLB().min();
            while (iter.hasNext()) {
                i = iter.nextInt();
                if (i < kerMin && !min.contains(i)) {
                    set.remove(i, this);
                }
            }
        }

        if (!set.getUB().isEmpty()) {
            int nextCard = setCard.nextValue(set.getLB().size());
            if (nextCard > 0 && nextCard <= setCard.getUB()) {
                int kerMin = set.getLB().isEmpty() || !setCard.contains(set.getLB().size()) ? Integer.MIN_VALUE : set.getLB().min();
                int remove = PropUtil.getEnv(set, set.getUB().size() - nextCard);
                remove = min.nextValue(remove);
                if (kerMin < remove && (defaultValue < remove || setCard.getLB() > 0)) {
                    min.updateUpperBound(remove - 1, this);
                } else {
                    while (remove <= min.getUB()) {
                        if (remove != kerMin) {
                            if (defaultValue != remove || setCard.getLB() > 0) {
                                min.removeValue(remove, this);
                            }
                        }
                        remove = min.nextValue(remove);
                    }
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (setCard.getLB() > 0 || !set.getLB().isEmpty() || !min.contains(defaultValue)) {
            int kerMin = Integer.MIN_VALUE;
            int index = set.getUB().size() - setCard.getLB();
            if (index >= 0 && index < set.getUB().size()) {
                kerMin = PropUtil.getEnv(set, index);
            }
            if (!set.getLB().isEmpty()) {
                kerMin = set.getLB().min();
            }
            if (kerMin != Integer.MIN_VALUE) {
                int prev = min.previousValue(kerMin + 1);
                int lb = min.getLB();
                while (prev >= lb && !set.getUB().contains(prev)) {
                    prev = min.previousValue(prev);
                }
                if (!set.getUB().contains(prev)) {
                    return ESat.FALSE;
                }
                if (min.isInstantiated() && kerMin == set.getUB().min()) {
                    return ESat.TRUE;
                }
            }
            if (!set.getUB().isEmpty()) {
                int nextCard = setCard.getLB();
                if (nextCard == 0) {
                    nextCard = setCard.nextValue(nextCard);
                }
                if (nextCard > 0 && nextCard <= setCard.getUB()) {
                    int remove = PropUtil.getEnv(set, set.getUB().size() - nextCard);
                    if (min.previousValue(remove + 1) < min.getLB()) {
                        return ESat.FALSE;
                    }
                }
            }
        }
        if (setCard.getUB() == 0 && min.isInstantiated()) {
            return ESat.eval(min.getValue() == defaultValue);
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "min(" + set + ", " + setCard + ", " + min + ")";
    }
}
