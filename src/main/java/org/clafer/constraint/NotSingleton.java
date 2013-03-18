package org.clafer.constraint;

import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractBinSetIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetVar;

/**
 *
 * @author jimmy
 */
public class NotSingleton extends AbstractBinSetIntSConstraint {

    public NotSingleton(SetVar set, IntDomainVar iv) {
        super(iv, set);
    }

    @Override
    public void propagate() throws ContradictionException {
        IntDomainVar card = v1.getCard();
        if (card.getSup() == 0 || card.getInf() > 1) {
            setEntailed();
        }
        if (card.isInstantiated() && card.getVal() == 1) {
            if (v0.isInstantiated()) {
                v1.remFromEnveloppe(v0.getVal(), this, false);
            }
            if (v1.isInstantiated()) {
                v0.removeVal(v1.getValue()[0], this, false);
            }
        }
    }

    @Override
    public boolean isSatisfied() {
        return v1.getValue().length != 1 || !v1.isInDomainKernel(v0.getVal());
    }
}
