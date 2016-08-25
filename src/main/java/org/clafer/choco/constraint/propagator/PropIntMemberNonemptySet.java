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
public class PropIntMemberNonemptySet extends Propagator<Variable> {

    private final IntVar element;
    private final SetVar set;
    private final IntVar setCard;

    public PropIntMemberNonemptySet(IntVar element, SetVar set, IntVar setCard) {
        super(new Variable[]{set, element, setCard}, PropagatorPriority.BINARY, false);
        this.element = element;
        this.set = set;
        this.setCard = setCard;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (!PropUtil.isDomIntersectKer(element, set)) {
            if (setCard.getLB() > 0) {
                setCard.updateLowerBound(set.getLB().size() + 1, this);
            }
            if (setCard.getUB() == set.getLB().size() + 1) {
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
        if (setCard.getLB() > 0) {
            PropUtil.domSubsetEnv(element, set, this);
            if (element.isInstantiated()) {
                set.force(element.getValue(), this);
                setPassive();
            }
        } else if (!PropUtil.isDomIntersectEnv(element, set)) {
            setCard.instantiateTo(0, this);
        } else {
//            PropUtil.domSubsetEnv(element, set, this);
        }
    }

    @Override
    public ESat isEntailed() {
        if (setCard.getLB() > 0) {
            if (!PropUtil.isDomIntersectEnv(element, set)) {
                return ESat.FALSE;
            }
            if (PropUtil.isDomSubsetKer(element, set)) {
                return ESat.TRUE;
            }
            return ESat.UNDEFINED;
        } else if (setCard.getUB() == 0) {
            return ESat.TRUE;
        } else if (setCard.getDomainSize() == 2
                && set.getUB().size() == setCard.getUB()
                && PropUtil.isDomSubsetEnv(element, set)) {
            return ESat.TRUE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return element + " in " + set + " with " + setCard + " if not empty";
    }
}
