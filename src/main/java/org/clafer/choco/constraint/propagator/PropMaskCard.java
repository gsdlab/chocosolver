package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.util.ESat;

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
        for (int i = set.getKernelFirst(); i != SetVar.END && i < to; i = set.getKernelNext()) {
            if (i >= from) {
                kerBetweenFromTo++;
            }
        }
        int kerOutsizeFromTo = set.getKernelSize() - kerBetweenFromTo;
        int envBetweenFromTo = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END && i < to; i = set.getEnvelopeNext()) {
            if (i >= from) {
                envBetweenFromTo++;
            }
        }
        int envOutsizeFromTo = set.getEnvelopeSize() - envBetweenFromTo;
        maskedCard.updateLowerBound(setCard.getLB() - envOutsizeFromTo, this);
        maskedCard.updateUpperBound(setCard.getUB() - kerOutsizeFromTo, this);
        setCard.updateLowerBound(maskedCard.getLB() + kerOutsizeFromTo, this);
        setCard.updateUpperBound(maskedCard.getUB() + envOutsizeFromTo, this);

        if (setCard.getLB() == maskedCard.getUB() + envOutsizeFromTo) {
            for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
                if (i < from || i >= to) {
                    set.addToKernel(i, this);
                }
            }
        }
        if (setCard.getUB() == maskedCard.getLB() + kerOutsizeFromTo) {
            for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
                if (i < from || i >= to) {
                    if (!set.kernelContains(i)) {
                        set.removeFromEnvelope(i, this);
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
