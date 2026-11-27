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
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 * The largest element in the set. Does nothing if the set is empty.
 *
 * TODO: constraint based on the cardinality of the set. For example:
 * env={0,1,2,3}, card={2,3} then clearly the largest element is at least 1.
 *
 * @author jimmy
 */
public class PropSetMax extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar setCard;
    private final IntVar max;

    public PropSetMax(SetVar set, IntVar setCard, IntVar max) {
        super(new Variable[]{set, setCard, max}, PropagatorPriority.UNARY, false);
        this.set = set;
        this.setCard = setCard;
        this.max = max;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isSetCardVar(int idx) {
        return idx == 1;
    }

    private boolean isMaxVar(int idx) {
        return idx == 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        if (isSetCardVar(vIdx)) {
            return IntEventType.INCLOW.getMask() + IntEventType.instantiation();
        }
        assert isMaxVar(vIdx);
        return IntEventType.all();
    }

    int in() {
        int in = Integer.MAX_VALUE;
        ISetIterator iter = set.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (max.contains(i)) {
                if (in != Integer.MAX_VALUE) {
                    return Integer.MAX_VALUE;
                }
                in = i;
            }
        }
        return in;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (setCard.getLB() > 0) {
            if (set.getUB().size() > 0) {
                max.updateUpperBound(set.getUB().max(), this);
            }
            int lim = setCard.getLB() - 1;
            if (lim < set.getUB().size()) {
                max.updateLowerBound(PropUtil.getEnv(set, lim), this);
            }
            if (set.getLB().size() > 0) {
                max.updateLowerBound(set.getLB().max(), this);
            }
            int in = in();
            if (in != Integer.MAX_VALUE) {
                max.instantiateTo(in, this);
            }
            int ub = max.getUB();
            ISetIterator iter = set.getUB().iterator();
            while (iter.hasNext()) {
                int i = iter.nextInt();
                if (i > ub) {
                    set.remove(i, this);
                }
            }
            if (max.isInstantiated()) {
                set.force(max.getValue(), this);
                setPassive();
            }
        } else if (setCard.getUB() > 0) {
            if (!PropUtil.isDomIntersectEnv(max, set)) {
                setCard.instantiateTo(0, this);
            } else if (set.getLB().size() > 0) {
                int m = max.getUB();
                int k = set.getLB().min();
                if (m < k) {
                    setCard.instantiateTo(0, this);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (setCard.getLB() > 0) {
            if (!PropUtil.isDomIntersectEnv(max, set)) {
                return ESat.FALSE;
            }
            if (set.getLB().size() > 0) {
                int m = max.getUB();
                int k = set.getLB().max();
                if (m < k) {
                    return ESat.FALSE;
                }
                int e = set.getUB().max();
                if (max.isInstantiated() && m == e && m == k) {
                    return ESat.TRUE;
                }
            }
        }
        return setCard.isInstantiatedTo(0) ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "max(" + set + ", " + setCard + ", " + max + ")";
    }
}
