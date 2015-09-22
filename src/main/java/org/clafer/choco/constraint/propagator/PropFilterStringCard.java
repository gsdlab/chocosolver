package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.util.ESat;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class PropFilterStringCard extends Propagator<IntVar> {

    private final IntVar setCard;
    private final IntVar[] result;

    public PropFilterStringCard(IntVar setCard, IntVar[] result) {
        super(Util.cons(setCard, result), PropagatorPriority.UNARY, false);
        this.setCard = setCard;
        this.result = result;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int i;
        for (i = 0; i < setCard.getLB(); i++) {
            result[i].removeValue(-1, this);
        }
        for (i = setCard.getUB(); i < result.length; i++) {
            result[i].instantiateTo(-1, this);
        }
        for (i = 0; i < result.length && !result[i].isInstantiatedTo(-1); i++) {
        }
        setCard.updateUpperBound(i, this);
        for (i = result.length - 1; i >= 0 && result[i].contains(-1); i--) {
        }
        setCard.updateLowerBound(i + 1, this);
    }

    @Override
    public ESat isEntailed() {
        return ESat.TRUE;
    }
}
