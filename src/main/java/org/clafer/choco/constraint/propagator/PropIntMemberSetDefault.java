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
 *
 * @author jimmy
 */
public class PropIntMemberSetDefault extends Propagator<Variable> {

    private final IntVar element;
    private final SetVar set;
    private final IntVar setCard;
    private final int defaultValue;

    public PropIntMemberSetDefault(IntVar element, SetVar set, IntVar setCard, int support) {
        super(new Variable[]{set, element, setCard}, PropagatorPriority.BINARY, false);
        this.element = element;
        this.set = set;
        this.setCard = setCard;
        this.defaultValue = support;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (!PropUtil.isDomIntersectKer(element, set)) {
            if (setCard.getLB() > 0 || !element.contains(defaultValue)) {
                setCard.updateLowerBound(set.getLB().size() + 1, this);
            }
            if (setCard.getUB() == set.getLB().size() + 1
                    && !PropUtil.isDomIntersectKer(element, set)) {
                // set can take only one more value, and currently does not intersect element.
                ISetIterator iter = set.getUB().iterator();
                while (iter.hasNext()) {
                    int i = iter.nextInt();
                    if (!set.getLB().contains(i) && !element.contains(i)) {
                        set.remove(i, this);
                    }
                }
            }
        }
        if (setCard.getLB() > 0 || !element.contains(defaultValue)) {
            PropUtil.domSubsetEnv(element, set, this);
            if (element.isInstantiated()) {
                set.force(element.getValue(), this);
                setPassive();
            }
        } else if (setCard.getUB() == 0 || !PropUtil.isDomIntersectEnv(element, set)) {
            element.instantiateTo(defaultValue, this);
            setCard.instantiateTo(0, this);
        } else {
            if (set.getUB().contains(defaultValue)) {
                PropUtil.domSubsetEnv(element, set, this);
            } else {
                int ub = element.getUB();
                for (int i = element.getLB(); i <= ub; i = element.nextValue(i)) {
                    if (i != defaultValue && !set.getUB().contains(i)) {
                        element.removeValue(i, this);
                    }
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (setCard.getLB() > 0 || !element.contains(defaultValue)) {
            if (!PropUtil.isDomIntersectEnv(element, set)) {
                return ESat.FALSE;
            }
            if (PropUtil.isDomSubsetKer(element, set)) {
                return ESat.TRUE;
            }
            return ESat.UNDEFINED;
        } else if (setCard.getUB() == 0) {
            return element.contains(defaultValue)
                    ? (element.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED)
                    : ESat.FALSE;
        } else if (element.isInstantiatedTo(defaultValue)
                && setCard.getDomainSize() == 2
                && set.getUB().contains(defaultValue)
                && set.getUB().size() == setCard.getUB()) {
            assert setCard.getLB() == 0;
            return ESat.TRUE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return element + " in " + set + " with " + setCard + " or else is " + defaultValue;
    }
}
