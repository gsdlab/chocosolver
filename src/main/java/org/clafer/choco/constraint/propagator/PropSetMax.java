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
        int in = SetVar.END;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (max.contains(i)) {
                if (in != SetVar.END) {
                    return SetVar.END;
                }
                in = i;
            }
        }
        return in;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (setCard.getLB() > 0) {
            if (set.getEnvelopeSize() > 0) {
                max.updateUpperBound(PropUtil.maxEnv(set), aCause);
            }
            int lim = setCard.getLB() - 1;
            if (lim < set.getEnvelopeSize()) {
                max.updateLowerBound(PropUtil.getEnv(set, lim), aCause);
            }
            if (set.getKernelSize() > 0) {
                max.updateLowerBound(PropUtil.maxKer(set), aCause);
            }
            int in = in();
            if (in != SetVar.END) {
                max.instantiateTo(in, aCause);
            }
            int ub = max.getUB();
            for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
                if (i > ub) {
                    set.removeFromEnvelope(i, aCause);
                }
            }
            if (max.isInstantiated()) {
                set.addToKernel(max.getValue(), aCause);
                setPassive();
            }
        } else if (setCard.getUB() > 0) {
            if (!PropUtil.isDomIntersectEnv(max, set)) {
                setCard.instantiateTo(0, aCause);
            } else if (set.getKernelSize() > 0) {
                int m = max.getUB();
                int k = PropUtil.minKer(set);
                if (m < k) {
                    setCard.instantiateTo(0, aCause);
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
            if (set.getKernelSize() > 0) {
                int m = max.getUB();
                int e = PropUtil.maxEnv(set);
                int k = PropUtil.maxKer(set);
                if (m < k) {
                    return ESat.FALSE;
                }
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
