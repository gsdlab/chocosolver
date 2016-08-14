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
 * The smallest element in the set. Does nothing if the set is empty.
 *
 * @author jimmy
 */
public class PropSetMin extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar setCard;
    private final IntVar min;

    public PropSetMin(SetVar set, IntVar setCard, IntVar min) {
        super(new Variable[]{set, setCard, min}, PropagatorPriority.UNARY, false);
        this.set = set;
        this.setCard = setCard;
        this.min = min;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isSetCardVar(int idx) {
        return idx == 1;
    }

    private boolean isMinVar(int idx) {
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
        assert isMinVar(vIdx);
        return IntEventType.all();
    }

    int in() {
        int in = Integer.MAX_VALUE;
        ISetIterator iter = set.getUB().iterator();
        while(iter.hasNext()) {
            int i = iter.nextInt();
            if (min.contains(i)) {
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
                min.updateLowerBound(set.getUB().min(), this);
            }
            int lim = set.getUB().size() - setCard.getLB();
            if (lim >= 0) {
                min.updateUpperBound(PropUtil.getEnv(set, lim), this);
            }
            if (set.getLB().size() > 0) {
                min.updateUpperBound(set.getLB().min(), this);
            }
            int in = in();
            if (in != Integer.MAX_VALUE) {
                min.instantiateTo(in, this);
            }
            int lb = min.getLB();
            ISetIterator iter = set.getUB().iterator();
            int i;
            while(iter.hasNext() && (i = iter.nextInt()) < lb) {
                set.remove(i, this);
            }
            if (min.isInstantiated()) {
                set.force(min.getValue(), this);
                setPassive();
            }
        } else if (setCard.getUB() > 0) {
            if (!PropUtil.isDomIntersectEnv(min, set)) {
                setCard.instantiateTo(0, this);
            } else if (set.getLB().size() > 0) {
                int m = min.getLB();
                int k = set.getLB().min();
                if (m > k) {
                    setCard.instantiateTo(0, this);
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (setCard.getLB() > 0) {
            if (!PropUtil.isDomIntersectEnv(min, set)) {
                return ESat.FALSE;
            }
            if (set.getLB().size() > 0) {
                int m = min.getLB();
                int k = set.getLB().min();
                if (m > k) {
                    return ESat.FALSE;
                }
                int e = set.getUB().min();
                if (min.isInstantiated() && m == e && m == k) {
                    return ESat.TRUE;
                }
            }
        }
        return setCard.isInstantiatedTo(0) ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "min(" + set + ", " + setCard + ", " + min + ")";
    }
}
