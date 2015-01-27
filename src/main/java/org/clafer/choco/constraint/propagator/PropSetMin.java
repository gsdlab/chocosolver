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
        int in = SetVar.END;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            if (min.contains(i)) {
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
                min.updateLowerBound(PropUtil.minEnv(set), aCause);
            }
            int lim = set.getEnvelopeSize() - setCard.getLB();
            if (lim >= 0) {
                min.updateUpperBound(PropUtil.getEnv(set, lim), aCause);
            }
            if (set.getKernelSize() > 0) {
                min.updateUpperBound(PropUtil.minKer(set), aCause);
            }
            int in = in();
            if (in != SetVar.END) {
                min.instantiateTo(in, aCause);
            }
            int lb = min.getLB();
            for (int i = set.getEnvelopeFirst(); i != SetVar.END && i < lb; i = set.getEnvelopeNext()) {
                set.removeFromEnvelope(i, aCause);
            }
            if (min.isInstantiated()) {
                set.addToKernel(min.getValue(), aCause);
                setPassive();
            }
        } else if (setCard.getUB() > 0) {
            if (!PropUtil.isDomIntersectEnv(min, set)) {
                setCard.instantiateTo(0, aCause);
            } else if (set.getKernelSize() > 0) {
                int m = min.getLB();
                int k = PropUtil.minKer(set);
                if (m > k) {
                    setCard.instantiateTo(0, aCause);
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
            if (set.getKernelSize() > 0) {
                int m = min.getLB();
                int e = PropUtil.minEnv(set);
                int k = PropUtil.minKer(set);
                if (m > k) {
                    return ESat.FALSE;
                }
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
