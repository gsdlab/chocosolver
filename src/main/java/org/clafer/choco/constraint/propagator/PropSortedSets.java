package org.clafer.choco.constraint.propagator;

import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.delta.SetDelta;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropSortedSets extends Propagator<SetVar> {

    private final SetVar[] sets;
    private final SetDeltaMonitor[] setsD;

    public PropSortedSets(SetVar[] sets) {
        super(sets, PropagatorPriority.QUADRATIC, true);
        this.sets = sets;
        this.setsD = PropUtil.monitorDeltas(sets, aCause);
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return EventType.ADD_TO_KER.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
    }

    @Override
    public void propagate(final int idxVarInProp, int mask) throws ContradictionException {
        final SetVar set = sets[idxVarInProp];
        final IntProcedure pickKerOnKer = new IntProcedure() {
            @Override
            public void execute(int ker) throws ContradictionException {
                int low = set.getKernelFirst();
                for (int i = low + 1; i < ker; i++) {
                    set.addToKernel(i, aCause);
                }
            }
        };
        final SetDeltaMonitor setD = setsD[idxVarInProp];
        setD.freeze();
        setD.forEach(pickKerOnKer, EventType.ADD_TO_KER);
        setD.unfreeze();
    }

    @Override
    public ESat isEntailed() {
        return ESat.TRUE;
    }
}
