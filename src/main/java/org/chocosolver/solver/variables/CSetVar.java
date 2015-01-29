package org.chocosolver.solver.variables;

import org.chocosolver.solver.constraints.set.SCF;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class CSetVar {

    private final SetVar set;
    private final IntVar card;

    public CSetVar(SetVar set, IntVar card) {
        this.set = set;
        this.card = card;

        if (!(set.isInstantiated() && card.isInstantiatedTo(set.getKernelSize()))) {
            card.getSolver().post(SCF.cardinality(set, card));
        }
    }

    public SetVar getSet() {
        return set;
    }

    public IntVar getCard() {
        return card;
    }

    @Override
    public String toString() {
        return "<" + set + ", " + card + ">";
    }

}
