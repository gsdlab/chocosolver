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
public class PropIntNotMemberSetCard extends Propagator<Variable> {

    private final IntVar element;
    private final SetVar set;
    private final IntVar setCard;

    public PropIntNotMemberSetCard(IntVar element, SetVar set, IntVar setCard) {
        super(new Variable[]{set, element, setCard}, PropagatorPriority.BINARY, false);
        this.element = element;
        this.set = set;
        this.setCard = setCard;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (setCard.contains(set.getUB().size())
                && PropUtil.isDomSubsetEnv(element, set)) {
            setCard.updateUpperBound(set.getUB().size() - 1, this);
        }
    }

    @Override
    public ESat isEntailed() {
        return ESat.TRUE;
    }
}
