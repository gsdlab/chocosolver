package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * <p>
 * Join a unary relation with a binary relation. The {@code take} variable is
 * the unary relation and the {@code children} variables are the binary
 * relation. The {@code to} variable is the result of the join.
 * </p>
 * <p>
 * Here is how the binary relation is encoded. Consider the relation:
 * {@code (0, 1), (0, 2), (1, 3), (2, 1)}. This is encoded as 3 different
 * {@code children} variables: {@code child0={1, 2}, child1={3}, child2={1}}.
 * </p>
 *
 * @author jimmy
 */
public class PropJoinRelation extends Propagator<SetVar> {

    private final SetVar take;
    private final SetDeltaMonitor takeD;
    private final SetVar[] children;
    private final SetDeltaMonitor[] childrenD;
    private final SetVar to;
    private final SetDeltaMonitor toD;

    public PropJoinRelation(SetVar take, SetVar[] children, SetVar to) {
        super(buildArray(take, to, children), PropagatorPriority.LINEAR, false);
        this.take = take;
        this.takeD = take.monitorDelta(aCause);
        this.children = children;
        this.childrenD = PropUtil.monitorDeltas(children, aCause);
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

    private boolean isChildVar(int idx) {
        return idx >= 2;
    }

    private int getChildVarIndex(int idx) {
        assert isChildVar(idx);
        return idx - 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
    }

    private void findMate(int toEnv) throws ContradictionException {
        boolean inKer = to.kernelContains(toEnv);
        int mate = -1;
        for (int j = take.getEnvelopeFirst(); j != SetVar.END; j = take.getEnvelopeNext()) {
            if (children[j].envelopeContains(toEnv)) {
                // Found a second mate.
                if (mate != -1 || !inKer) {
                    mate = -2;
                    break;
                }
                mate = j;
            }
        }
        if (mate == -1) {
            // No mates.
            to.removeFromEnvelope(toEnv, aCause);
        } else if (mate != -2 && inKer) {
            // One mate.
            take.addToKernel(mate, aCause);
            PropUtil.kerSubsetKer(children[mate], to, aCause);
            PropUtil.envSubsetEnv(children[mate], to, aCause);
            children[mate].addToKernel(toEnv, aCause);
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Pick to and prune child
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            PropUtil.kerSubsetKer(children[i], to, aCause);
            PropUtil.envSubsetEnv(children[i], to, aCause);
        }

        // Prune take
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (!PropUtil.isKerSubsetEnv(children[i], to)) {
                take.removeFromEnvelope(i, aCause);
            }
        }

        // Pick take, pick child, pick take, prune to
        for (int i = to.getEnvelopeFirst(); i != SetVar.END; i = to.getEnvelopeNext()) {
            findMate(i);
        }
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
            if ((EventType.REMOVE_FROM_ENVELOPE.mask & mask) != 0) {
                TIntArrayList removed = null;
                for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
                    if (!PropUtil.isKerSubsetEnv(children[i], to)) {
                        take.removeFromEnvelope(i, aCause);
                        // Cannot call findMate here because we inside iterating take env.
                        // Queue up the even to do later.
                        if (removed == null) {
                            removed = new TIntArrayList(1);
                        }
                        removed.add(i);
                    }
                }
                if (removed != null) {
                    TIntIterator it = removed.iterator();
                    while (it.hasNext()) {
                        SetVar child = children[it.next()];
                        for (int i = child.getEnvelopeFirst(); i != SetVar.END; i = child.getEnvelopeNext()) {
                            if (to.envelopeContains(i)) {
                                findMate(i);
                            }
                        }
                    }
                }
            }
        } else {
            assert isChildVar(idxVarInProp);
            final int id = getChildVarIndex(idxVarInProp);
            childrenD[id].freeze();
            childrenD[id].forEach(pruneToOnChildEnv, EventType.REMOVE_FROM_ENVELOPE);
            if (take.envelopeContains(id)) {
                if (take.kernelContains(id)) {
                    childrenD[id].forEach(pickToOnChildKer, EventType.ADD_TO_KER);
                } else {
                    IntProcedure pruneTakeOnChildKer = new IntProcedure() {
                        @Override
                        public void execute(int childKer) throws ContradictionException {
                            if (!to.envelopeContains(childKer)) {
                                take.removeFromEnvelope(id, aCause);
                                for (int i = children[id].getEnvelopeFirst(); i != SetVar.END; i = children[id].getEnvelopeNext()) {
                                    if (to.envelopeContains(i)) {
                                        findMate(i);
                                    }
                                }
                            }
                        }
                    };
                    childrenD[id].forEach(pruneTakeOnChildKer, EventType.ADD_TO_KER);
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
                if (to.envelopeContains(i)) {
                    findMate(i);
                }
            }
        }
    };
    private final IntProcedure pickToAndPruneChildOnTakeKer = new IntProcedure() {
        @Override
        public void execute(int takeKer) throws ContradictionException {
            assert take.kernelContains(takeKer);

            SetVar child = children[takeKer];
            PropUtil.kerSubsetKer(child, to, aCause);
            PropUtil.envSubsetEnv(child, to, aCause);
        }
    };
    private final IntProcedure pruneToOnChildEnv = new IntProcedure() {
        @Override
        public void execute(int childEnv) throws ContradictionException {
            // Note the the child may no longer be in take, but still need to find mate.
            // For example:
            //     take = {0,1,2}, child0 = {0}, child1 = {1}, child2 = {2}, to = {0,1,2}
            //   remove 2 from take and 2 from child2
            //     take = {0,1}, child0 = {0}, child1 = {1}, child2 = {}, to = {0,1,2}
            // Need to find mate for 2 on child2 or else to will keep 2, and break
            // idempotency.
            findMate(childEnv);
        }
    };
    private final IntProcedure pickToOnChildKer = new IntProcedure() {
        @Override
        public void execute(int childKer) throws ContradictionException {
            // assert id in ker(take)
            to.addToKernel(childKer, aCause);
        }
    };
    private final IntProcedure pruneChildOnToEnv = new IntProcedure() {
        @Override
        public void execute(int toEnv) throws ContradictionException {
            assert !to.envelopeContains(toEnv);

            for (int takeKer = take.getKernelFirst(); takeKer != SetVar.END; takeKer = take.getKernelNext()) {
                children[takeKer].removeFromEnvelope(toEnv, aCause);
            }
        }
    };
    private final IntProcedure pickTakeOnToKer = new IntProcedure() {
        @Override
        public void execute(int toVal) throws ContradictionException {
            assert to.kernelContains(toVal);
            findMate(toVal);
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

    @Override
    public String toString() {
        return "joinRelation(" + take + ", " + Arrays.toString(children) + ", " + to + ")";
    }
}
