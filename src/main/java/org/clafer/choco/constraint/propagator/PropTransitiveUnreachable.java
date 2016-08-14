package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 *
 * @author jimmy
 */
public class PropTransitiveUnreachable extends Propagator<SetVar> {

    private static final long serialVersionUID = 1L;

    private final ISetDeltaMonitor[] relationD;

    public PropTransitiveUnreachable(SetVar[] relation) {
        super(relation, PropagatorPriority.LINEAR, true);
        this.relationD = PropUtil.monitorDeltas(relation, this);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
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
        ISetIterator jIter = var.getLB().iterator();
        while (jIter.hasNext()) {
            int j = jIter.nextInt();
            if (i != j) {
                ISetIterator kIter = vars[j].getUB().iterator();
                while (kIter.hasNext()) {
                    int k = kIter.nextInt();
                    if (!var.getUB().contains(k) && vars[j].remove(k, this)) {
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
        assert var.getLB().contains(j);
        ISetIterator iter = var.getUB().iterator();
        while (iter.hasNext()) {
            int k = iter.nextInt();
            if (!var.getUB().contains(k)) {
                vars[j].remove(k, this);
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
        assert !var.getUB().contains(k);
        ISetIterator iter = var.getLB().iterator();
        while (iter.hasNext()) {
            int j = iter.nextInt();
            if (i != j && vars[j].remove(k, this)) {
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
            if (vars[i].getLB().contains(k)) {
                backPointers[bs++] = i;
            } else if (!vars[i].getUB().contains(k)) {
                noBackPointers[nbs++] = i;
            }
        }
        for (int ii = 0; ii < nbs; ii++) {
            int i = noBackPointers[ii];
            for (int jj = 0; jj < bs; jj++) {
                int j = backPointers[jj];
                if (vars[i].remove(j, this)) {
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
        assert vars[j].getLB().contains(k);
        for (int i = 0; i < vars.length; i++) {
            if (i != j && !vars[i].getUB().contains(k) && vars[i].remove(j, this)) {
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
        assert !var.getUB().contains(k);
        for (int j = 0; j < vars.length; j++) {
            if (i != j && vars[j].getLB().contains(k) && var.remove(j, this)) {
                onRemoveEnv(i, j);
            }
        }
    }

    private void onRemoveEnv(int i, int k) throws ContradictionException {
        assert !vars[i].getUB().contains(k);
        forwardPruneIK(i, k);
        backwardPruneIK(i, k);
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (SetVar var : vars) {
            ISetIterator iter = var.getUB().iterator();
            while (iter.hasNext()) {
                int j = iter.nextInt();
                if (j < 0 || j >= vars.length) {
                    var.remove(j, this);
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
        relationD[i].forEach(j -> {
            forwardPruneIJ(i, j);
            backwardPruneJK(i, j);
        }, SetEventType.ADD_TO_KER);
        relationD[i].forEach(k -> onRemoveEnv(i, k), SetEventType.REMOVE_FROM_ENVELOPE);
        relationD[i].unfreeze();
    }

    @Override
    public ESat isEntailed() {
        // PropTransitive has it taken care of.
        return ESat.TRUE;
    }

    @Override
    public String toString() {
        return "transitiveUnreachable(" + Arrays.toString(vars) + ")";
    }
}
