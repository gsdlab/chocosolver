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
 *
 * @author jimmy
 */
public class PropContainsImpliesEqualCard2 extends Propagator<Variable> {

    private final SetVar subset;
    private final IntVar subsetCard;
    private final SetVar superset;

    public PropContainsImpliesEqualCard2(SetVar subset, IntVar subsetCard, SetVar superset) {
        super(new Variable[]{subsetCard, superset}, PropagatorPriority.UNARY, false);
        this.subset = subset;
        this.subsetCard = subsetCard;
        this.superset = superset;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (vIdx == 0) {
            return IntEventType.INSTANTIATE.getMask() + IntEventType.DECUPP.getMask();
        }
        assert vIdx == 1;
        return SetEventType.ADD_TO_KER.getMask();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (subsetCard.getUB() == superset.getLB().size()) {
            PropUtil.envSubsetKer(subset, superset, this);
        }
        if (subsetCard.getUB() <= superset.getLB().size()) {
            setPassive();
        }
    }

    @Override
    public ESat isEntailed() {
        if (subsetCard.getLB() > superset.getUB().size()) {
            return ESat.FALSE;
        }
        if (subsetCard.getUB() <= superset.getLB().size()) {
            return ESat.TRUE;
        }
        return ESat.UNDEFINED;
    }
}
