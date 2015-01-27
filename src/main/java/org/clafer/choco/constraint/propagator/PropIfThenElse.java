package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropIfThenElse extends Propagator<BoolVar> {

    private final BoolVar antecedent, consequent, alternative;

    public PropIfThenElse(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
        super(new BoolVar[]{antecedent, consequent, alternative}, PropagatorPriority.UNARY, true);
        this.antecedent = antecedent;
        this.consequent = consequent;
        this.alternative = alternative;
    }

    private boolean isAntecedent(int idx) {
        return idx == 0;
    }

    private boolean isConsequent(int idx) {
        return idx == 1;
    }

    private boolean isAlternative(int idx) {
        return idx == 2;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return IntEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (antecedent.isInstantiated()) {
            if (antecedent.getValue() == 1) {
                consequent.setToTrue(aCause);
            } else {
                alternative.setToTrue(aCause);
            }
            setPassive();
        } else if (consequent.isInstantiatedTo(1) && alternative.isInstantiatedTo(1)) {
            setPassive();
        } else {
            if (consequent.isInstantiatedTo(0)) {
                antecedent.setToFalse(aCause);
                alternative.setToTrue(aCause);
            }
            if (alternative.isInstantiatedTo(0)) {
                antecedent.setToTrue(aCause);
                consequent.setToTrue(aCause);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isAntecedent(idxVarInProp)) {
            assert antecedent.isInstantiated();
            if (antecedent.getValue() == 1) {
                consequent.setToTrue(aCause);
            } else {
                alternative.setToTrue(aCause);
            }
            setPassive();
        } else if (isConsequent(idxVarInProp)) {
            assert consequent.isInstantiated();
            if (consequent.getValue() == 0) {
                antecedent.setToFalse(aCause);
                alternative.setToTrue(aCause);
            } else if (alternative.isInstantiatedTo(1)) {
                setPassive();
            }
        } else {
            assert isAlternative(idxVarInProp);
            if (alternative.getValue() == 0) {
                antecedent.setToTrue(aCause);
                consequent.setToTrue(aCause);
            } else if (consequent.isInstantiatedTo(1)) {
                setPassive();
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (antecedent.isInstantiated()) {
            return antecedent.getValue() == 1 ? consequent.getBooleanValue() : alternative.getBooleanValue();
        }
        if (consequent.isInstantiatedTo(1) && alternative.isInstantiatedTo(1)) {
            return ESat.TRUE;
        }
        if (consequent.isInstantiatedTo(0) && alternative.isInstantiatedTo(0)) {
            return ESat.FALSE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "if " + antecedent + " then " + consequent + " else " + alternative;
    }
}
