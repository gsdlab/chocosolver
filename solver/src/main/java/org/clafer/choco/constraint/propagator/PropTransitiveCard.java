package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
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
public class PropTransitiveCard extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar[] cards;

    public PropTransitiveCard(SetVar set, IntVar[] cards) {
        super(buildArray(set, cards), PropagatorPriority.LINEAR, false);
        this.set = set;
        this.cards = cards;
    }

    private static Variable[] buildArray(SetVar set, IntVar[] cards) {
        Variable[] array = new Variable[cards.length + 2];
        array[0] = set;
        array[1] = set.getCard();
        System.arraycopy(cards, 0, array, 2, cards.length);
        return array;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        ISetIterator iter = set.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (i < 0 || i >= cards.length) {
                fails();
            } else {
                set.getCard().updateLowerBound(cards[i].getLB(), this);
                cards[i].updateUpperBound(set.getCard().getUB(), this);
            }
        }
    }

    @Override
    public ESat isEntailed() {
        ISetIterator iter = set.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (i < 0 || i >= cards.length || cards[i].getLB() > set.getCard().getUB()) {
                return ESat.FALSE;
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "transitiveCard(" + Arrays.toString(vars) + ")";
    }
}
