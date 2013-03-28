package org.clafer.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.collection.Appender;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.IntConstraintFactory;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.constraints.set.SetConstraintsFactory;
import solver.exception.ContradictionException;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.set.SetSearchStrategy;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.VariableFactory;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * Note: Assumes disjoint children!
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
        // Prune to and child
        TIntHashSet viableTo = new TIntHashSet();
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            ConstraintUtil.iterateEnv(children[i], viableTo);
        }
        ConstraintUtil.subsetEnv(to, viableTo, aCause);

        // Pick to and prune child
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            ConstraintUtil.subsetKer(children[i], to, aCause);
            ConstraintUtil.subsetEnv(children[i], to, aCause);
        }

        // Pick take
        
        takeD.unfreeze();
        ConstraintUtil.unfreezeAll(childrenD);
        toD.unfreeze();
        
        assert !ESat.FALSE.equals(isEntailed());
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isTakeVar(idxVarInProp)) {
            takeD.freeze();
            takeD.forEach(pruneToOnTakeEnv, EventType.REMOVE_FROM_ENVELOPE);
            takeD.forEach(pickToAndPruneChildOnTakeKer, EventType.ADD_TO_KER);
            takeD.unfreeze();
        } else if (isToVar(idxVarInProp)) {
            toD.freeze();
            toD.forEach(pruneChildOnToEnv, EventType.REMOVE_FROM_ENVELOPE);
            toD.forEach(pickTakeOnToKer, EventType.ADD_TO_KER);
            toD.unfreeze();
        } else {
            assert isChildVar(idxVarInProp);
            int id = getChildVarIndex(idxVarInProp);
            if (take.kernelContains(id)) {
                childrenD[id].freeze();
                childrenD[id].forEach(pruneToOnChildEnv, EventType.REMOVE_FROM_ENVELOPE);
                childrenD[id].forEach(pickToOnChildKer, EventType.ADD_TO_KER);
                childrenD[id].unfreeze();
            }
        }
        assert !ESat.FALSE.equals(isEntailed());
    }
    private final IntProcedure pruneToOnTakeEnv = new IntProcedure() {

        @Override
        public void execute(int takeEnv) throws ContradictionException {
            assert !take.envelopeContains(takeEnv);
            for (int i = children[takeEnv].getEnvelopeFirst(); i != SetVar.END; i = children[takeEnv].getEnvelopeNext()) {
                if (!hasChildSupport(i)) {
                    to.removeFromEnvelope(i, aCause);
                }
            }
        }
    };

    private boolean hasChildSupport(int child) {
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (children[i].envelopeContains(child)) {
                return true;
            }
        }
        return false;
    }
    private final IntProcedure pickToAndPruneChildOnTakeKer = new IntProcedure() {

        @Override
        public void execute(int takeKer) throws ContradictionException {
            assert take.kernelContains(takeKer);

            SetVar child = children[takeKer];
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
            if (child == -1) {
                contradiction(to, "no support for " + toVal);
            } else {
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
        int count = 0;
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            for (int j = children[i].getEnvelopeFirst(); j != SetVar.END; j = children[i].getEnvelopeNext()) {
                if (!to.envelopeContains(j)) {
                    return ESat.FALSE;
                }
                count++;
            }
        }
        if (count < to.getKernelSize()) {
            return ESat.FALSE;
        }
        return count == to.getKernelSize() ? ESat.TRUE : ESat.UNDEFINED;
    }

    public static void main(String[] args) {
        Solver solver = new Solver();

//        SearchMonitorFactory.log(solver, false, true);

        SetVar take = VariableFactory.set("take", new int[]{0, 1, 2}, solver);
        SetVar[] children = new SetVar[3];
        for (int i = 0; i < children.length; i++) {
            children[i] = VariableFactory.set("child" + i, new int[]{0, 1, 2, 3, 4}, solver);
        }
        SetVar to = VariableFactory.set("to", new int[]{0, 1, 2, 3, 4}, solver);

        SetVar[] svs = Appender.<SetVar>build().add(take).addAll(children).add(to).toArray();
        Constraint con = new Constraint(svs, solver);
        con.setPropagators(new PropJoin(take, children, to));
        solver.post(con);

        solver.post(SetConstraintsFactory.all_disjoint(children));

        solver.set(new SetSearchStrategy(svs));

        if (solver.findSolution()) {
            do {
                if (!ESat.TRUE.equals(solver.isEntailed())) {
                    System.out.println(solver);
                    throw new Error();
                }
            } while (solver.nextSolution());
        }
        System.out.println(solver.getMeasures());

    }
}
