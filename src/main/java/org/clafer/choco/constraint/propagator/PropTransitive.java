package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropTransitive extends Propagator<SetVar> {

    private static final long serialVersionUID = 1L;

    private final ISetDeltaMonitor[] relationD;

    public PropTransitive(SetVar[] relation) {
        super(relation, PropagatorPriority.LINEAR, true);
        this.relationD = PropUtil.monitorDeltas(relation, this);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        // TODO ENV
        return SetEventType.ADD_TO_KER.getMask();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = 0; i < vars.length; i++) {
            SetVar var = vars[i];
            for (int j = var.getEnvelopeFirst(); j != SetVar.END; j = var.getEnvelopeNext()) {
                if (j < 0 || j >= vars.length) {
                    var.removeFromEnvelope(j, this);
                }
            }
            for (int j = var.getKernelFirst(); j != SetVar.END; j = var.getKernelNext()) {
                if (i != j) {
                    PropUtil.kerSubsetKer(vars[j], var, this);
                }
            }
            for (int j = 0; j < i; j++) {
                assert i != j;
                if (vars[j].kernelContains(i)) {
                    PropUtil.kerSubsetKer(var, vars[j], this);
                }
            }
        }
    }

    /*
     * if
     *   i ---> j
     *   j ---> k
     * then
     *   i ---> k
     */
    private void prune(int i, int j, boolean first) throws ContradictionException {
        assert vars[i].kernelContains(j);
        if (i != j) {
            if (PropUtil.kerSubsetKer(vars[j], vars[i], this) || first) {
                for (int k = 0; k < vars.length; k++) {
                    if (i != k && vars[k].kernelContains(i)) {
                        if (vars[k].addToKernel(j, this)) {
                            prune(k, j, false);
                        }
                    }
                }
            }
        }
    }

    @Override
    public void propagate(final int i, int mask) throws ContradictionException {
        relationD[i].freeze();
        relationD[i].forEach(j -> prune(i, j, true), SetEventType.ADD_TO_KER);
        relationD[i].unfreeze();
    }

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        for (int i = 0; i < vars.length; i++) {
            SetVar var = vars[i];
            allInstantiated &= var.isInstantiated();
            for (int j = var.getKernelFirst(); j != SetVar.END; j = var.getKernelNext()) {
                if (j < 0 || j >= vars.length) {
                    return ESat.FALSE;
                }
                if (i != j && !PropUtil.isKerSubsetEnv(vars[j], var)) {
                    return ESat.FALSE;
                }
            }
        }
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "transitive(" + Arrays.toString(vars) + ")";
    }
}
