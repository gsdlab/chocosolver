package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropTransitiveUnreachable extends Propagator<SetVar> {

    private static final long serialVersionUID = 1L;

    private final ISetDeltaMonitor[] relationD;

    public PropTransitiveUnreachable(SetVar[] relation) {
        super(relation, PropagatorPriority.LINEAR, true);
        this.relationD = PropUtil.monitorDeltas(relation, aCause);
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return SetEventType.all();
    }

    /*
     * if
     *   i ---> j
     *   i -/-> k
     * then
     *   j -/-> k
     */
    private void forwardPrune(int i) throws ContradictionException {
        SetVar var = vars[i];
        for (int j = var.getKernelFirst(); j != SetVar.END; j = var.getKernelNext()) {
            if (i != j) {
                for (int k = vars[j].getEnvelopeFirst(); k != SetVar.END; k = vars[j].getEnvelopeNext()) {
                    if (!var.envelopeContains(k) && vars[j].removeFromEnvelope(k, aCause)) {
                        onRemoveEnv(j, k);
                    }
                }
            }
        }
    }

    /*
     * if
     *   i ---> j
     *   i -/-> k
     * then
     *   j -/-> k
     */
    private void forwardPruneIJ(int i, int j) throws ContradictionException {
        SetVar var = vars[i];
        assert var.kernelContains(j);
        for (int k = vars[j].getEnvelopeFirst(); k != SetVar.END; k = vars[j].getEnvelopeNext()) {
            if (!var.envelopeContains(k)) {
                vars[j].removeFromEnvelope(k, aCause);
                onRemoveEnv(j, k);
            }
        }
    }

    /*
     * if
     *   i ---> j
     *   i -/-> k
     * then
     *   j -/-> k
     */
    private void forwardPruneIK(int i, int k) throws ContradictionException {
        SetVar var = vars[i];
        assert !var.envelopeContains(k);
        for (int j = var.getKernelFirst(); j != SetVar.END; j = var.getKernelNext()) {
            if (i != j && vars[j].removeFromEnvelope(k, aCause)) {
                onRemoveEnv(j, k);
            }
        }
    }

    /*
     * if
     *   i -/-> k
     *   j ---> k
     * then
     *   i -/-> j
     */
    private void backwardPrune(int k) throws ContradictionException {
        int[] backPointers = new int[vars.length];
        int[] noBackPointers = new int[vars.length];
        int bs = 0;
        int nbs = 0;
        for (int i = 0; i < vars.length; i++) {
            if (vars[i].kernelContains(k)) {
                backPointers[bs++] = i;
            } else if (!vars[i].envelopeContains(k)) {
                noBackPointers[nbs++] = i;
            }
        }
        for (int ii = 0; ii < nbs; ii++) {
            int i = noBackPointers[ii];
            for (int jj = 0; jj < bs; jj++) {
                int j = backPointers[jj];
                if (vars[i].removeFromEnvelope(j, aCause)) {
                    onRemoveEnv(i, j);
                }
            }
        }
    }

    /*
     * if
     *   i -/-> k
     *   j ---> k
     * then
     *   i -/-> j
     */
    private void backwardPruneJK(int j, int k) throws ContradictionException {
        assert vars[j].kernelContains(k);
        for (int i = 0; i < vars.length; i++) {
            if (i != j && !vars[i].envelopeContains(k) && vars[i].removeFromEnvelope(j, aCause)) {
                onRemoveEnv(i, j);
            }
        }
    }

    /*
     * if
     *   i -/-> k
     *   j ---> k
     * then
     *   i -/-> j
     */
    private void backwardPruneIK(int i, int k) throws ContradictionException {
        SetVar var = vars[i];
        assert !var.envelopeContains(k);
        for (int j = 0; j < vars.length; j++) {
            if (i != j && vars[j].kernelContains(k) && var.removeFromEnvelope(j, aCause)) {
                onRemoveEnv(i, j);
            }
        }
    }

    private void onRemoveEnv(int i, int k) throws ContradictionException {
        assert !vars[i].envelopeContains(k);
        forwardPruneIK(i, k);
        backwardPruneIK(i, k);
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (SetVar var : vars) {
            for (int j = var.getEnvelopeFirst(); j != SetVar.END; j = var.getEnvelopeNext()) {
                if (j < 0 || j >= vars.length) {
                    var.removeFromEnvelope(j, aCause);
                }
            }
        }
        for (int i = 0; i < vars.length; i++) {
            forwardPrune(i);
            backwardPrune(i);
        }
    }

    @Override
    public void propagate(final int i, int mask) throws ContradictionException {
        relationD[i].freeze();
        relationD[i].forEach(new IntProcedure() {

            private static final long serialVersionUID = 1L;

            @Override
            public void execute(int j) throws ContradictionException {
                forwardPruneIJ(i, j);
                backwardPruneJK(i, j);
            }
        }, SetEventType.ADD_TO_KER);
        relationD[i].forEach(new IntProcedure() {

            private static final long serialVersionUID = 1L;

            @Override
            public void execute(int k) throws ContradictionException {
                onRemoveEnv(i, k);
            }
        }, SetEventType.REMOVE_FROM_ENVELOPE);
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
        return "transitiveUnreachable(" + Arrays.toString(vars) + ")";
    }
}
