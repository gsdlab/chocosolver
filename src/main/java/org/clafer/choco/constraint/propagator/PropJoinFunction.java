package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import java.util.Arrays;
import memory.structure.IndexedBipartiteSet;
import org.clafer.collection.MutableBoolean;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import solver.variables.delta.IIntDeltaMonitor;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.ESat;
import util.procedure.IntProcedure;

/**
 * <p>
 * Join a unary set relation with a binary function. This propagator is a
 * specialized version of {@link PropJoinRelation}. The {@code take} variable is
 * the unary relation and the {@code ref} variables are the function. The
 * {@code to} variable is the result of the join.
 * </p>
 * <p>
 * Here is how the binary function is encoded. Consider the function:
 * {@code (0, 1), (1, 3), (2, 1)}. This is encoded as 3 different {@code ref}
 * variables: {@code ref0={1}, ref1={3}, ref2={1}}.
 * </p>
 *
 * @author jimmy
 */
public class PropJoinFunction extends Propagator<Variable> {

    private final SetVar take;
    private final SetDeltaMonitor takeD;
    private final IndexedBipartiteSet dontCare;
    private final IntVar[] refs;
    private final IIntDeltaMonitor[] refsD;
    private final SetVar to;
    private final SetDeltaMonitor toD;

    public PropJoinFunction(SetVar take, IntVar[] refs, SetVar to) {
        super(buildArray(take, to, refs), PropagatorPriority.QUADRATIC, true);
        this.take = take;
        this.takeD = take.monitorDelta(aCause);
        this.dontCare = new IndexedBipartiteSet(take.getSolver().getEnvironment(), PropUtil.iterateEnv(take));
        this.refs = refs;
        this.refsD = PropUtil.monitorDeltas(refs, aCause);
        this.to = to;
        this.toD = to.monitorDelta(aCause);
    }

    private static Variable[] buildArray(SetVar take, SetVar to, IntVar[] refs) {
        Variable[] array = new Variable[refs.length + 2];
        array[0] = take;
        array[1] = to;
        System.arraycopy(refs, 0, array, 2, refs.length);
        return array;
    }

    private boolean isTakeVar(int idx) {
        return idx == 0;
    }

    private boolean isToVar(int idx) {
        return idx == 1;
    }

    private boolean isRefVar(int idx) {
        return idx >= 2;
    }

    private int getRefVarIndex(int idx) {
        assert isRefVar(idx);
        return idx - 2;
    }

    @Override
    public boolean advise(int idxVarInProp, int mask) {
        if (isRefVar(idxVarInProp)) {
            return dontCare.contains(getRefVarIndex(idxVarInProp));
        }
        return super.advise(idxVarInProp, mask);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isTakeVar(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
        }
        if (isToVar(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
        }
        assert isRefVar(vIdx);
        return EventType.INT_ALL_MASK();
    }

    private boolean findMate(int toEnv) throws ContradictionException {
        boolean inKer = to.kernelContains(toEnv);
        int mate = -1;
        for (int j = take.getEnvelopeFirst(); j != SetVar.END; j = take.getEnvelopeNext()) {
            if (refs[j].contains(toEnv)) {
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
            return refs[mate].instantiateTo(toEnv, aCause);
        }
        return false;
    }

    private void findMates() throws ContradictionException {
        for (int i = to.getEnvelopeFirst(); i != SetVar.END; i = to.getEnvelopeNext()) {
            if (findMate(i)) {
                findMates();
                return;
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune take
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (i < 0 || i >= refs.length || !PropUtil.isDomIntersectEnv(refs[i], to)) {
                take.removeFromEnvelope(i, aCause);
                dontCare.remove(i);
            }
        }

        // Pick to and prune refs
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            PropUtil.domSubsetEnv(refs[i], to, aCause);
            if (refs[i].instantiated()) {
                int value = refs[i].getValue();
                to.addToKernel(value, aCause);
            }
        }

        // Prune to
        findMates();
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
            toD.forEach(pruneRefOnToEnv, EventType.REMOVE_FROM_ENVELOPE);
            toD.forEach(pickTakeOnToKer, EventType.ADD_TO_KER);
            toD.unfreeze();
            if ((EventType.REMOVE_FROM_ENVELOPE.mask & mask) != 0) {
                TIntArrayList removed = null;
                for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
                    if (!PropUtil.isDomIntersectEnv(refs[i], to)) {
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
                        IntVar ref = refs[it.next()];
                        int ub = ref.getUB();
                        for (int i = ref.getLB(); i <= ub; i = ref.nextValue(i)) {
                            if (to.envelopeContains(i)) {
                                if (findMate(i)) {
                                    findMates();
                                }
                            }
                        }
                    }
                }
            }
        } else {
            assert isRefVar(idxVarInProp);
            final int id = getRefVarIndex(idxVarInProp);
            final IIntDeltaMonitor refD = refsD[id];
            final MutableBoolean bool = new MutableBoolean();
            refD.freeze();
            IntProcedure pruneToOnRefRem = new IntProcedure() {
                @Override
                public void execute(int refRem) throws ContradictionException {
                    if (bool.isClear()) {
                        if (to.envelopeContains(refRem)) {
                            if (findMate(refRem)) {
                                bool.set();
                                findMates();
                            }
                        }
                    }
                }
            };
            refD.forEach(pruneToOnRefRem, EventType.REMOVE);
            refD.unfreeze();
            IntVar ref = refs[id];
            if (EventType.isRemove(mask)) {
                if (!PropUtil.isDomIntersectEnv(ref, to)) {
                    take.removeFromEnvelope(id, aCause);
                    dontCare.remove(id);
                }
            }
            if (ref.instantiated()) {
                if (take.kernelContains(id)) {
                    to.addToKernel(ref.getValue(), aCause);
                }
            }
        }
    }
    private final IntProcedure pruneToOnTakeEnv = new IntProcedure() {
        @Override
        public void execute(int takeEnv) throws ContradictionException {
            assert !take.envelopeContains(takeEnv);

            dontCare.remove(takeEnv);
            IntVar ref = refs[takeEnv];
            int ub = ref.getUB();
            for (int i = ref.getLB(); i <= ub; i = ref.nextValue(i)) {
                if (to.envelopeContains(i)) {
                    if (findMate(i)) {
                        findMates();
                        return;
                    }
                }
            }
        }
    };
    private final IntProcedure pickToAndPruneChildOnTakeKer = new IntProcedure() {
        @Override
        public void execute(int takeKer) throws ContradictionException {
            assert take.kernelContains(takeKer);

            IntVar ref = refs[takeKer];
            PropUtil.domSubsetEnv(ref, to, aCause);
            if (ref.instantiated()) {
                to.addToKernel(ref.getValue(), aCause);
            }
        }
    };
    private final IntProcedure pruneRefOnToEnv = new IntProcedure() {
        @Override
        public void execute(int toEnv) throws ContradictionException {
            assert !to.envelopeContains(toEnv);

            for (int takeKer = take.getKernelFirst(); takeKer != SetVar.END; takeKer = take.getKernelNext()) {
                IntVar ref = refs[takeKer];
                if (ref.removeValue(toEnv, aCause) && ref.instantiated()) {
                    to.addToKernel(ref.getValue(), aCause);
                }
            }
        }
    };
    private final IntProcedure pickTakeOnToKer = new IntProcedure() {
        @Override
        public void execute(int toVal) throws ContradictionException {
            assert to.kernelContains(toVal);
            if (findMate(toVal)) {
                findMates();
            }
        }
    };

    @Override
    public ESat isEntailed() {
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            if (i < 0 || i >= refs.length || !PropUtil.isDomIntersectEnv(refs[i], to)) {
                return ESat.FALSE;
            }
            if (refs[i].instantiated()) {
                int value = refs[i].getValue();
                if (!to.envelopeContains(value)) {
                    return ESat.FALSE;
                }
            }
        }
        boolean completelyInstantiated = take.instantiated() && to.instantiated();
        int count = 0;
        IntVar[] taken = new IntVar[take.getEnvelopeSize()];
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (i >= 0 && i < refs.length) {
                IntVar ref = refs[i];
                completelyInstantiated = completelyInstantiated && ref.instantiated();
                taken[count++] = ref;
            }
        }
        if (count < taken.length) {
            taken = Arrays.copyOf(taken, count);
        }
        for (int i = to.getKernelFirst(); i != SetVar.END; i = to.getKernelNext()) {
            if (!PropUtil.domsContain(taken, i)) {
                return ESat.FALSE;
            }
        }
        return completelyInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "joinFunction(" + take + ", " + Arrays.toString(refs) + ", " + to + ")";
    }
}
