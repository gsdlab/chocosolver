package org.clafer.constraint;

import gnu.trove.set.hash.TIntHashSet;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.SetVarImpl;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropJoin extends Propagator<SetVar> {

    private final SetVar take;
    private final SetDeltaMonitor takeD;
    private final SetVar[] children;
    private final SetDeltaMonitor[] childrenD;
    private final SetVar to;
    private final SetDeltaMonitor toD;

    public PropJoin(SetVar take, SetVar[] children, SetVar to) {
        super(buildArray(take, to, children), PropagatorPriority.BINARY);
        this.take = take;
        this.takeD = take.monitorDelta(aCause);
        this.children = children;
        this.childrenD = ConstraintUtil.monitorDeltas(children, aCause);
        this.to = to;
        this.toD = to.monitorDelta(aCause);
    }

    private static SetVar[] buildArray(SetVar take, SetVar to, SetVar[] children) {
        SetVar[] array = new SetVar[children.length + 2];
        array[0] = take;
        array[1] = to;
        System.arraycopy(children, 0, array, 2, children.length);
        return array;
    }

    private boolean isTakeVar(int idx) {
        return idx == 0;
    }

    private boolean isToVar(int idx) {
        return idx == 1;
    }

    private boolean isChildVar(int varIdx) {
        return varIdx >= 2;
    }

    private int getChildVarIndex(int varIdx) {
        return varIdx - 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask + EventType.INSTANTIATE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        takeD.unfreeze();
        ConstraintUtil.unfreezeAll(childrenD);
        toD.unfreeze();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
//        if (isTakeVar(idxVarInProp)) {
//            takeD.freeze();
//            takeD.forEach(pickToAndPruneChildOnTakeKer, EventType.ADD_TO_KER);
//            takeD.unfreeze();
//        } else if (isToVar(idxVarInProp)) {
//            toD.freeze();
//            toD.forEach(pruneChildOnToEnv, EventType.REMOVE_FROM_ENVELOPE);
//            toD.forEach(pickTakeOnToKer, EventType.ADD_TO_KER);
//            toD.unfreeze();
//        } else {
//            assert isChildVar(idxVarInProp);
//            int id = getChildVarIndex(idxVarInProp);
//            if (take.kernelContains(id)) {
//                childrenD[id].freeze();
//                childrenD[id].forEach(pruneToOnChildEnv, EventType.REMOVE_FROM_ENVELOPE);
//                childrenD[id].forEach(pickToOnChildKer, EventType.ADD_TO_KER);
//                childrenD[id].unfreeze();
//            }
//        }
        if (take.instantiated()) {
            TIntHashSet v = new TIntHashSet();
            for (int takeEnv = take.getEnvelopeFirst(); takeEnv != SetVar.END; takeEnv = take.getEnvelopeNext()) {
                if (!children[takeEnv].instantiated()) {
                    return;
                }
                ConstraintUtil.iterateEnv(children[takeEnv], v);
            }
            to.instantiateTo(v.toArray(), aCause);
        }
    }
    private final IntProcedure pickToAndPruneChildOnTakeKer = new IntProcedure() {

        @Override
        public void execute(int takeVal) throws ContradictionException {
            assert take.kernelContains(takeVal);

            SetVar child = children[takeVal];
            ConstraintUtil.subsetKer(child, to, aCause);
            ConstraintUtil.subsetEnv(child, to, aCause);
        }
    };
    private final IntProcedure pruneToOnChildEnv = new IntProcedure() {

        @Override
        public void execute(int i) throws ContradictionException {
            for (int takeEnv = take.getEnvelopeFirst(); takeEnv != SetVar.END; takeEnv = take.getEnvelopeNext()) {
                if (children[takeEnv].envelopeContains(i)) {
                    return;
                }
            }
            to.removeFromEnvelope(i, aCause);
        }
    };
    private final IntProcedure pickToOnChildKer = new IntProcedure() {

        @Override
        public void execute(int i) throws ContradictionException {
            to.addToKernel(i, aCause);
        }
    };
    private final IntProcedure pruneChildOnToEnv = new IntProcedure() {

        @Override
        public void execute(int i) throws ContradictionException {
            assert !to.envelopeContains(i);

            for (int takeKer = take.getKernelFirst(); takeKer != SetVar.END; takeKer = take.getKernelNext()) {
                children[takeKer].removeFromEnvelope(i, aCause);
            }
        }
    };
    private final IntProcedure pickTakeOnToKer = new IntProcedure() {

        @Override
        public void execute(int toVal) throws ContradictionException {
            assert to.kernelContains(toVal);

            // Even though the children are disjoint, there env may not be.
            int child = -1;
            for (int takeEnv = take.getEnvelopeFirst(); takeEnv != SetVar.END; takeEnv = take.getEnvelopeNext()) {
                if (children[takeEnv].envelopeContains(toVal)) {
                    if (child != -1) {
                        // Found a second child.
                        return;
                    }
                    child = takeEnv;
                }
            }
            if (child != -1) {
                take.addToKernel(child, aCause);
                ConstraintUtil.subsetKer(children[child], to, aCause);
                children[child].addToKernel(toVal, aCause);
            }
        }
    };

    @Override
    public boolean isStateLess() {
        return super.isStateLess();
    }

    @Override
    public ESat isEntailed() {
        return ESat.UNDEFINED;
    }
}
