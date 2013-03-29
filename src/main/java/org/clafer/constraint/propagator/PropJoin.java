package org.clafer.constraint.propagator;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.clafer.collection.Appender;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.constraints.set.SetConstraintsFactory;
import solver.exception.ContradictionException;
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
        this.childrenD = PropagatorUtil.monitorDeltas(children, aCause);
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
        return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune to and child
        TIntHashSet viableTo = new TIntHashSet();
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            PropagatorUtil.iterateEnv(children[i], viableTo);
        }
        PropagatorUtil.subsetEnv(to, viableTo, aCause);

        // Pick to and prune child
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            PropagatorUtil.subsetKer(children[i], to, aCause);
            PropagatorUtil.subsetEnv(children[i], to, aCause);
        }
        // Pick take
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
            childrenD[id].freeze();
            if (take.envelopeContains(id)) {
                childrenD[id].forEach(pruneToOnChildEnv, EventType.REMOVE_FROM_ENVELOPE);
                if (take.kernelContains(id)) {
                    childrenD[id].forEach(pickToOnChildKer, EventType.ADD_TO_KER);
                }
            }
            childrenD[id].unfreeze();
        }
    }
    private final IntProcedure pruneToOnTakeEnv = new IntProcedure() {

        @Override
        public void execute(int takeEnv) throws ContradictionException {
            assert !take.envelopeContains(takeEnv);
            for (int i = children[takeEnv].getEnvelopeFirst(); i != SetVar.END; i = children[takeEnv].getEnvelopeNext()) {
                if (!to.envelopeContains(i)) {
                    continue;
                }
                int child = -1;
                for (int j = take.getEnvelopeFirst(); j != SetVar.END; j = take.getEnvelopeNext()) {
                    if (children[j].envelopeContains(i)) {
                        // Found a second or don't care after first
                        if (child != -1 || !to.kernelContains(i)) {
                            child = -2;
                            break;
                        }
                        child = j;
                    }
                }
                if (child == -1) {
                    // i is not longer supported
                    to.removeFromEnvelope(i, aCause);
                } else if (child != -2 && to.kernelContains(i)) {
                    // i has only one support
                    take.addToKernel(child, aCause);
                    PropagatorUtil.subsetKer(children[child], to, aCause);
                    PropagatorUtil.subsetEnv(children[child], to, aCause);
                    children[child].addToKernel(i, aCause);
                }
            }
        }
    };
    private final IntProcedure pickToAndPruneChildOnTakeKer = new IntProcedure() {

        @Override
        public void execute(int takeKer) throws ContradictionException {
            assert take.kernelContains(takeKer);

            SetVar child = children[takeKer];
            PropagatorUtil.subsetKer(child, to, aCause);
            PropagatorUtil.subsetEnv(child, to, aCause);
        }
    };
    private final IntProcedure pruneToOnChildEnv = new IntProcedure() {

        @Override
        public void execute(int i) throws ContradictionException {
            if (!to.envelopeContains(i)) {
                return;
            }
            int child = -1;
            for (int takeEnv = take.getEnvelopeFirst(); takeEnv != SetVar.END; takeEnv = take.getEnvelopeNext()) {
                if (children[takeEnv].envelopeContains(i)) {
                    // Found a second or don't care after first
                    if (child != -1 || !to.kernelContains(i)) {
                        return;
                    }
                    child = takeEnv;
                }
            }
            if (child == -1) {
                // No support
                to.removeFromEnvelope(i, aCause);
            } else if (to.kernelContains(i)) {
                // One support
                take.addToKernel(child, aCause);
                PropagatorUtil.subsetKer(children[child], to, aCause);
                PropagatorUtil.subsetEnv(children[child], to, aCause);
                children[child].addToKernel(i, aCause);
            }
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
                PropagatorUtil.subsetKer(children[child], to, aCause);
                PropagatorUtil.subsetEnv(children[child], to, aCause);
                children[child].addToKernel(toVal, aCause);
            }
        }
    };

    @Override
    public ESat isEntailed() {
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            for (int j = children[i].getKernelFirst(); j != SetVar.END; j = children[i].getKernelNext()) {
                if (!to.envelopeContains(j)) {
                    return ESat.FALSE;
                }
            }
        }
        int count = 0;
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            count += children[i].getEnvelopeSize();
        }
        if (count < to.getKernelSize()) {
            return ESat.FALSE;
        }
        if (!take.instantiated() || !to.instantiated()) {
            return ESat.UNDEFINED;
        }
        for (SetVar child : children) {
            if (!child.instantiated()) {
                return ESat.UNDEFINED;
            }
        }
        return ESat.TRUE;
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

    @Override
    public String toString() {
        return "propJoin(" + take + ", " + Arrays.toString(children) + ", " + to + ")";
    }
}
