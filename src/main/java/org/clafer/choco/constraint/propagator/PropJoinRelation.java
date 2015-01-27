package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import java.util.Arrays;
import org.chocosolver.memory.structure.IndexedBipartiteSet;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

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
    private final ISetDeltaMonitor takeD;
    private final IndexedBipartiteSet dontCare;
    private final SetVar[] children;
    private final ISetDeltaMonitor[] childrenD;
    private final SetVar to;
    private final ISetDeltaMonitor toD;

    public PropJoinRelation(SetVar take, SetVar[] children, SetVar to) {
        super(buildArray(take, to, children), PropagatorPriority.QUADRATIC, true);
        this.take = take;
        this.takeD = take.monitorDelta(aCause);
        this.dontCare = new IndexedBipartiteSet(take.getSolver().getEnvironment(), PropUtil.iterateEnv(take));
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
    public boolean advise(int idxVarInProp, int mask) {
        if (isChildVar(idxVarInProp)) {
            return dontCare.contains(getChildVarIndex(idxVarInProp));
        }
        return super.advise(idxVarInProp, mask);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return SetEventType.all();
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
        // Prune take
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (i < 0 || i >= children.length || !PropUtil.isKerSubsetEnv(children[i], to)) {
                take.removeFromEnvelope(i, aCause);
                dontCare.remove(i);
            }
        }

        // Pick to and prune child
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            PropUtil.kerSubsetKer(children[i], to, aCause);
            PropUtil.envSubsetEnv(children[i], to, aCause);
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
            takeD.forEach(pruneToOnTakeEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            takeD.forEach(pickToAndPruneChildOnTakeKer, SetEventType.ADD_TO_KER);
            takeD.unfreeze();
        } else if (isToVar(idxVarInProp)) {
            toD.freeze();
            toD.forEach(pruneChildOnToEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            toD.forEach(pickTakeOnToKer, SetEventType.ADD_TO_KER);
            toD.unfreeze();
            if (SetEventType.isEnvRemoval(mask)) {
                TIntArrayList removed = null;
                for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
                    if (!PropUtil.isKerSubsetEnv(children[i], to)) {
                        take.removeFromEnvelope(i, aCause);
                        dontCare.remove(i);
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
            final ISetDeltaMonitor childD = childrenD[id];
            childD.freeze();
            // Note that we MUST prune even if id is not in env(take) to ensure
            // idempotence. Otherwise if id is removed from take and val removed
            // from child at the same time is no longer supported, we need to
            // remove val from to as well.
            childD.forEach(pruneToOnChildEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            if (take.kernelContains(id)) {
                childD.forEach(pickToOnChildKer, SetEventType.ADD_TO_KER);
            } else if (take.envelopeContains(id)) {
                IntProcedure pruneTakeOnChildKer = new IntProcedure() {
                    @Override
                    public void execute(int childKer) throws ContradictionException {
                        if (!to.envelopeContains(childKer)) {
                            take.removeFromEnvelope(id, aCause);
                            dontCare.remove(id);
                            for (int i = children[id].getEnvelopeFirst(); i != SetVar.END; i = children[id].getEnvelopeNext()) {
                                if (to.envelopeContains(i)) {
                                    findMate(i);
                                }
                            }
                        }
                    }
                };
                childD.forEach(pruneTakeOnChildKer, SetEventType.ADD_TO_KER);
            }
            childD.unfreeze();
        }
    }
    private final IntProcedure pruneToOnTakeEnv = new IntProcedure() {
        @Override
        public void execute(int takeEnv) throws ContradictionException {
            assert !take.envelopeContains(takeEnv);
            dontCare.remove(takeEnv);
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
            if (to.envelopeContains(childEnv)) {
                findMate(childEnv);
            }
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
            if (i < 0 || i >= children.length) {
                return ESat.FALSE;
            }
            for (int j = children[i].getKernelFirst(); j != SetVar.END; j = children[i].getKernelNext()) {
                if (!to.envelopeContains(j)) {
                    return ESat.FALSE;
                }
            }
        }
        boolean completelyInstantiated = take.isInstantiated() && to.isInstantiated();
        int count = 0;
        SetVar[] taken = new SetVar[take.getEnvelopeSize()];
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (i >= 0 && i < children.length) {
                SetVar child = children[i];
                completelyInstantiated = completelyInstantiated && child.isInstantiated();
                taken[count++] = child;
            }
        }
        if (count < taken.length) {
            taken = Arrays.copyOf(taken, count);
        }
        for (int i = to.getKernelFirst(); i != SetVar.END; i = to.getKernelNext()) {
            if (!PropUtil.envsContain(taken, i)) {
                return ESat.FALSE;
            }
        }
        return completelyInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "joinRelation(" + take + ", " + Arrays.toString(children) + ", " + to + ")";
    }
}
