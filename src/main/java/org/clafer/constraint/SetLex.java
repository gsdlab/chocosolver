package org.clafer.constraint;

import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.set.AbstractLargeSetSConstraint;
import choco.kernel.solver.variables.set.SetVar;
import org.clafer.Check;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class SetLex extends AbstractLargeSetSConstraint {

    private final Integer low;
    private final Integer high;

    public SetLex(SetVar[] vars, Card card) {
        super(vars);
        if (vars.length < 1) {
            throw new IllegalArgumentException();
        }
        Check.notNull(card);
        this.low = card.getLow();
        this.high = card.getHigh();
    }

    @Override
    public void propagate() throws ContradictionException {
//        pruneInf(0);
        pruneSup();
    }

    private void pruneInf(int varIdx) throws ContradictionException {
        if (low != 0) {
            for (int i = varIdx + 1; i < vars.length; i++) {
                int lastInf = vars[i - 1].getEnveloppeInf() + low;
                for (int j = vars[i].getEnveloppeInf(); j < lastInf; j++) {
                    vars[i].remFromEnveloppe(j, this, false);
                }
            }
        }
    }

    private void pruneSup() throws ContradictionException {
        if (high != Integer.MAX_VALUE) {
            int start = vars[0].getEnveloppeDomainSize() > 0 ? vars[0].getEnveloppeSup() + 1 : 0;
            for (int i = 1; i < vars.length; i++) {
                if (vars[i].getEnveloppeDomainSize() > 0) {
                    final int sup = vars[i].getEnveloppeSup();
                    for (int j = start + high; j <= sup; j++) {
                        vars[i].remFromEnveloppe(j, this, false);
                    }
                    start = Math.min(start + high, sup + 1);
                }
            }
        }
    }

    @Override
    public boolean isConsistent() {
        throw new Error();
    }

    @Override
    public boolean isSatisfied() {
        return true;
    }
}
