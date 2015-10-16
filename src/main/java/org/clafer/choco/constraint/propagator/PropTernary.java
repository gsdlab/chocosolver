package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropTernary extends Propagator<IntVar> {

    private final BoolVar antecedent;
    private final IntVar result, consequent, alternative;

    public PropTernary(BoolVar antecedent, IntVar result, IntVar consequent, IntVar alternative) {
        super(new IntVar[]{antecedent, result, consequent, alternative}, PropagatorPriority.BINARY, false);
        this.antecedent = antecedent;
        this.result = result;
        this.consequent = consequent;
        this.alternative = alternative;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (!antecedent.isInstantiated()) {
            int ub = result.getUB();
            boolean intersects1 = false;
            boolean intersects2 = false;
            for (int i = result.getLB(); i <= ub; i = result.nextValue(i)) {
                boolean contains1 = consequent.contains(i);
                boolean contains2 = alternative.contains(i);
                intersects1 |= contains1;
                intersects2 |= contains2;
                if (!contains1 && !contains2) {
                    result.removeValue(i, this);
                }
            }
            assert intersects1 || intersects2;
            if (!intersects1) {
                antecedent.setToFalse(this);
            } else if (!intersects2) {
                antecedent.setToTrue(this);
            }
        }
        if (antecedent.isInstantiated()) {
            if (antecedent.getValue() == 1) {
                PropUtil.domSubsetDom(result, consequent, this);
                PropUtil.domSubsetDom(consequent, result, this);
            } else {
                PropUtil.domSubsetDom(result, alternative, this);
                PropUtil.domSubsetDom(alternative, result, this);
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (antecedent.isInstantiated()) {
            if (antecedent.getValue() == 1) {
                return PropUtil.isDomIntersectDom(result, consequent)
                        ? (result.isInstantiated() && consequent.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED)
                        : ESat.FALSE;
            } else {
                return PropUtil.isDomIntersectDom(result, alternative)
                        ? (result.isInstantiated() && alternative.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED)
                        : ESat.FALSE;
            }
        }
        return PropUtil.isDomIntersectDom(result, consequent) || PropUtil.isDomIntersectDom(result, alternative)
                ? ESat.UNDEFINED : ESat.FALSE;
    }
}
