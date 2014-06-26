package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.delta.ISetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropTransitive extends Propagator<SetVar> {

    private static final long serialVersionUID = 1L;

    private final ISetDeltaMonitor[] relationD;

    public PropTransitive(SetVar[] relation) {
        super(relation, PropagatorPriority.LINEAR, true);
        this.relationD = PropUtil.monitorDeltas(relation, aCause);
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return EventType.ADD_TO_KER.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = 0; i < vars.length; i++) {
            SetVar var = vars[i];
            for (int j = var.getEnvelopeFirst(); j != SetVar.END; j = var.getEnvelopeNext()) {
                if (j < 0 || j >= vars.length) {
                    var.removeFromEnvelope(j, aCause);
                }
            }
            for (int j = var.getKernelFirst(); j != SetVar.END; j = var.getKernelNext()) {
                if (i != j) {
                    PropUtil.kerSubsetKer(vars[j], var, aCause);
                }
            }
            for (int j = 0; j < i; j++) {
                assert i != j;
                if (vars[j].kernelContains(i)) {
                    PropUtil.kerSubsetKer(var, vars[j], aCause);
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
            if (PropUtil.kerSubsetKer(vars[j], vars[i], aCause) || first) {
                for (int k = 0; k < vars.length; k++) {
                    if (i != k && vars[k].kernelContains(i)) {
                        if (vars[k].addToKernel(j, aCause)) {
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
        relationD[i].forEach(new IntProcedure() {

            private static final long serialVersionUID = 1L;

            @Override
            public void execute(int j) throws ContradictionException {
                prune(i, j, true);
            }
        }, EventType.ADD_TO_KER);
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
