package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 * TODO: implement as two reifysetequal && result subset (consequent union alternative)
 *  
 * @author jimmy
 */
public class PropSetTernary extends Propagator<Variable> {

    private final BoolVar antecedent;
    private final SetVar result, consequent, alternative;

    public PropSetTernary(BoolVar antecedent, SetVar result, SetVar consequent, SetVar alternative) {
        super(new Variable[]{antecedent, result, consequent, alternative}, PropagatorPriority.BINARY, false);
        this.antecedent = antecedent;
        this.result = result;
        this.consequent = consequent;
        this.alternative = alternative;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (!antecedent.isInstantiated()) {
            if (!PropUtil.isKerSubsetEnv(result, consequent)
                    || !PropUtil.isKerSubsetEnv(consequent, result)) {
                antecedent.instantiateTo(0, this);
            } else if (!PropUtil.isKerSubsetEnv(result, alternative)
                    || !PropUtil.isKerSubsetEnv(alternative, result)) {
                antecedent.instantiateTo(1, this);
            } else {
                ISetIterator iter = result.getUB().iterator();
                while (iter.hasNext()) {
                    int i = iter.nextInt();
                    if (!consequent.getUB().contains(i) && !alternative.getUB().contains(i)) {
                        result.remove(i, this);
                    } else if (consequent.getLB().contains(i) && alternative.getLB().contains(i)) {
                        result.force(i, this);
                    }
                }
            }
        }
        if (antecedent.isInstantiated()) {
            if (antecedent.getValue() == 1) {
                PropUtil.envSubsetEnv(result, consequent, this);
                PropUtil.envSubsetEnv(consequent, result, this);
                PropUtil.kerSubsetKer(result, consequent, this);
                PropUtil.kerSubsetKer(consequent, result, this);
            } else {
                PropUtil.envSubsetEnv(result, alternative, this);
                PropUtil.envSubsetEnv(alternative, result, this);
                PropUtil.kerSubsetKer(result, alternative, this);
                PropUtil.kerSubsetKer(alternative, result, this);
            }
        }
    }

    @Override
    public ESat isEntailed() {
        boolean possibleTrue
                = PropUtil.isKerSubsetEnv(result, consequent)
                && PropUtil.isKerSubsetEnv(consequent, result);
        boolean possibleFalse
                = PropUtil.isKerSubsetEnv(result, alternative)
                && PropUtil.isKerSubsetEnv(alternative, result);
        if (!possibleTrue && !possibleFalse) {
            return ESat.FALSE;
        }
        if (antecedent.isInstantiated()) {
            if (antecedent.getValue() == 1) {
                if (!possibleTrue) {
                    return ESat.FALSE;
                }
                return result.isInstantiated() && consequent.isInstantiated()
                        ? ESat.TRUE : ESat.UNDEFINED;
            } else {
                if (!possibleFalse) {
                    return ESat.FALSE;
                }
                return result.isInstantiated() && alternative.isInstantiated()
                        ? ESat.TRUE : ESat.UNDEFINED;
            }
        }
        if (possibleTrue && possibleFalse
                && result.isInstantiated()
                && consequent.isInstantiated()
                && alternative.isInstantiated()) {
            return ESat.TRUE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return result + " = " + antecedent + " ? " + consequent + " : " + alternative;
    }
}
