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
public class PropIntMemberSetCard extends Propagator<Variable> {

    private final IntVar element;
    private final SetVar set;
    private final IntVar setCard;

    public PropIntMemberSetCard(IntVar element, SetVar set, IntVar setCard) {
        super(new Variable[]{set, element, setCard}, PropagatorPriority.BINARY, false);
        this.element = element;
        this.set = set;
        this.setCard = setCard;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (!PropUtil.isDomIntersectKer(element, set)) {
            setCard.updateLowerBound(set.getLB().size() + 1, this);
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
        } else {
            setCard.updateLowerBound(1, this);
        }
    }

    @Override
    public ESat isEntailed() {
        return ESat.TRUE;
    }
}
