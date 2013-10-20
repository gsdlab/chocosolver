package org.clafer.choco.constraint.propagator;

import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.BoolVar;
import solver.variables.EventType;
import util.ESat;

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
        return EventType.INT_ALL_MASK();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (antecedent.instantiated()) {
            if (antecedent.getValue() == 1) {
                consequent.setToTrue(aCause);
            } else {
                alternative.setToTrue(aCause);
            }
            setPassive();
        } else if (consequent.instantiatedTo(1) && alternative.instantiatedTo(1)) {
            setPassive();
        } else {
            if (consequent.instantiatedTo(0)) {
                antecedent.setToFalse(aCause);
                alternative.setToTrue(aCause);
            }
            if (alternative.instantiatedTo(0)) {
                antecedent.setToTrue(aCause);
                consequent.setToTrue(aCause);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isAntecedent(idxVarInProp)) {
            assert antecedent.instantiated();
            if (antecedent.getValue() == 1) {
                consequent.setToTrue(aCause);
            } else {
                alternative.setToTrue(aCause);
            }
            setPassive();
        } else if (isConsequent(idxVarInProp)) {
            assert consequent.instantiated();
            if (consequent.getValue() == 0) {
                antecedent.setToFalse(aCause);
                alternative.setToTrue(aCause);
            } else if (alternative.instantiatedTo(1)) {
                setPassive();
            }
        } else {
            assert isAlternative(idxVarInProp);
            if (alternative.getValue() == 0) {
                antecedent.setToTrue(aCause);
                consequent.setToTrue(aCause);
            } else if (consequent.instantiatedTo(1)) {
                setPassive();
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (antecedent.instantiated()) {
            return antecedent.getValue() == 1 ? consequent.getBooleanValue() : alternative.getBooleanValue();
        }
        if (consequent.instantiatedTo(1) && alternative.instantiatedTo(1)) {
            return ESat.TRUE;
        }
        if (consequent.instantiatedTo(0) && alternative.instantiatedTo(0)) {
            return ESat.FALSE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "if " + antecedent + " then " + consequent + " else " + alternative;
    }
}
