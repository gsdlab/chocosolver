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
public class PropMaskCard extends Propagator<Variable> {

    private final SetVar set;
    private final IntVar setCard;
    private final IntVar maskedCard;
    // Inclusive
    private final int from;
    // Exclusive
    private final int to;

    public PropMaskCard(SetVar set, IntVar setCard, SetVar masked, IntVar maskedCard, int from, int to) {
        super(new Variable[]{set, setCard, maskedCard}, PropagatorPriority.LINEAR, false);

        if (from > to) {
            throw new IllegalArgumentException();
        }

        this.set = set;
        this.setCard = setCard;
        this.maskedCard = maskedCard;
        this.from = from;
        this.to = to;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int kerBetweenFromTo = 0;
        ISetIterator setKer = set.getLB().iterator();
        while (setKer.hasNext()) {
            int i = setKer.nextInt();
            if (i >= from && i < to) {
                kerBetweenFromTo++;
            }
        }
        int kerOutsizeFromTo = set.getLB().size() - kerBetweenFromTo;
        int envBetweenFromTo = 0;
        ISetIterator setEnv = set.getUB().iterator();
        while (setEnv.hasNext()) {
            int i = setEnv.nextInt();
            if (i >= from && i < to) {
                envBetweenFromTo++;
            }
        }
        int envOutsizeFromTo = set.getUB().size() - envBetweenFromTo;
        maskedCard.updateLowerBound(setCard.getLB() - envOutsizeFromTo, this);
        maskedCard.updateUpperBound(setCard.getUB() - kerOutsizeFromTo, this);
        setCard.updateLowerBound(maskedCard.getLB() + kerOutsizeFromTo, this);
        setCard.updateUpperBound(maskedCard.getUB() + envOutsizeFromTo, this);

        if (setCard.getLB() == maskedCard.getUB() + envOutsizeFromTo) {
            setEnv.reset();
            while (setEnv.hasNext()) {
                int i = setEnv.nextInt();
                if (i < from || i >= to) {
                    set.force(i, this);
                }
            }
        }
        if (setCard.getUB() == maskedCard.getLB() + kerOutsizeFromTo) {
            setEnv.reset();
            while (setEnv.hasNext()) {
                int i = setEnv.nextInt();
                if (i < from || i >= to) {
                    if (!set.getLB().contains(i)) {
                        set.remove(i, this);
                    }
                }
            }
        }
    }

    @Override
    public ESat isEntailed() {
        return ESat.TRUE;
    }
}
