package org.clafer.choco.constraint.propagator;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.queue.TIntQueue;
import gnu.trove.stack.TIntStack;
import gnu.trove.stack.array.TIntArrayStack;
import java.util.Arrays;
import org.clafer.collection.CircularIntStack;
import org.clafer.collection.FixedCapacityIntSet;
import solver.Configuration;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
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
    private final IntVar[] refs;
    private final IIntDeltaMonitor[] refsD;
    private final SetVar to;
    private final SetDeltaMonitor toD;

    public PropJoinFunction(SetVar take, IntVar[] refs, SetVar to) {
        super(buildArray(take, to, refs), PropagatorPriority.LINEAR, false);
        this.take = take;
        this.takeD = take.monitorDelta(aCause);
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

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune to
        // Need to iterate env(take) many times so read it into an array
        boolean changed;
        do {
            changed = false;
            for (int i = to.getEnvelopeFirst(); i != SetVar.END; i = to.getEnvelopeNext()) {
                changed |= findMate(i);
            }
        } while (changed);

        int sameRefs = 0;
        FixedCapacityIntSet set = new FixedCapacityIntSet(take.getEnvelopeSize());
        // Prune refs, Pick to
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            PropUtil.intSubsetEnv(refs[i], to, aCause);
            if (refs[i].instantiated()) {
                int value = refs[i].getValue();
                sameRefs += (set.add(value)) ? 0 : 1;
                to.addToKernel(value, aCause);
            }
        }
//        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
//            if (!refs[i].instantiated() && PropUtil.isDomainSubsetOf(refs[i], set)) {
//                sameRefs++;
//            }
//        }

        // Prune take
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (!PropUtil.domainIntersectEnv(refs[i], to)) {
                take.removeFromEnvelope(i, aCause);
            }
        }

        int minTakeSize = sameRefs + to.getKernelSize();
//        System.out.println("##" + to.getKernelSize() + " : " + minTakeSize + " : " + take.getEnvelopeSize() + " : " + evtmask + " : " + set + " : " + this);
        if (minTakeSize == take.getEnvelopeSize()) {
            take.instantiateTo(PropUtil.iterateEnv(take), aCause);
            to.instantiateTo(PropUtil.iterateKer(to), aCause);
//            for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
//                if (!refs[i].instantiated()) {
//                    PropUtil.intSubsetEnv(refs[i], to, aCause);
//                    if (refs[i].instantiated()) {
//                        int value = refs[i].getValue();
//                        if (!set.add(value) || !to.kernelContains(value)) {
//                            // sameRefs++ || to.getKernelSize++
//                            contradiction(take, "Take too small");
//                        }
//                    }
//                }
//            }
//            CircularIntStack queue = new CircularIntStack(refs.length);
//            queue.addAll(set);
//            while (!queue.isEmpty()) {
//                int remove = queue.pop();
////                System.out.print(remove);
//                for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
//                    if (!refs[i].instantiated() && !PropUtil.isDomainSubsetOf(refs[i], set)) {
//                        refs[i].removeValue(remove, aCause);
//                        if (refs[i].instantiated()) {
//                            int value = refs[i].getValue();
//                            if (!set.add(value) || !to.kernelContains(value)) {
//                                // sameRefs++ || to.getKernelSize++
//                                contradiction(take, "Take too small");
//                            }
//                            queue.push(value);
//                        }
//                    }
//                }
//            }
//            sameRefs = 0;
//            set.clear();
//            for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
//                if (refs[i].instantiated()) {
//                    int value = refs[i].getValue();
//                    sameRefs += (set.add(value)) ? 0 : 1;
//                    to.addToKernel(value, aCause);
//                }
//            }
//            for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
//                if (!refs[i].instantiated() && PropUtil.isDomainSubsetOf(refs[i], set)) {
//                    sameRefs++;
//                }
//            }
        }
//        minTakeSize = sameRefs + to.getKernelSize();
//        System.out.println("----" + to.getKernelSize() + " : " + minTakeSize + " : " + take.getEnvelopeSize() + " : " + evtmask + " : " + set + " : " + this);
        if (minTakeSize > take.getEnvelopeSize()) {
            contradiction(take, "To too large");
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.ADD_TO_KER);
//        if (isTakeVar(idxVarInProp)) {
//            if ((mask & EventType.REMOVE_FROM_ENVELOPE.mask) != 0) {
//                // Prune to
//                int[] takeEnv = PropUtil.iterateEnv(take);
//                for (int i = to.getEnvelopeFirst(); i != SetVar.END; i = to.getEnvelopeNext()) {
//                    if (!possibleTo(takeEnv, i)) {
//                        to.removeFromEnvelope(i, aCause);
//                    }
//                }
//            }
//            takeD.freeze();
//            takeD.forEach(pruneRefAndPickToOnTakeKer, EventType.ADD_TO_KER);
//            takeD.unfreeze();
//        } else if (isToVar(idxVarInProp)) {
//            toD.freeze();
//            toD.forEach(pruneRefAndPickToOnToEnv, EventType.REMOVE_FROM_ENVELOPE);
//            toD.unfreeze();
//            if ((mask & EventType.REMOVE_FROM_ENVELOPE.mask) != 0) {
//                // Prune take
//                for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
//                    if (!PropUtil.domainIntersectEnv(refs[i], to)) {
//                        take.removeFromEnvelope(i, aCause);
//                    }
//                }
//            }
//        } else {
//            assert isRefVar(idxVarInProp);
//            int id = getRefVarIndex(idxVarInProp);
//
//            refsD[id].freeze();
//            if (take.envelopeContains(id)) {
//                refsD[id].forEach(pruneToOnRefRem, EventType.REMOVE);
//            }
//            refsD[id].unfreeze();
//
//            // Prune refs, Pick to
//            for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
//                PropUtil.intSubsetEnv(refs[i], to, aCause);
//                if (refs[i].instantiated()) {
//                    to.addToKernel(refs[i].getValue(), aCause);
//                }
//            }
//
//            // Prune take
//            for (int i : PropUtil.iterateEnv(take)) {
//                if (!PropUtil.domainIntersectEnv(refs[i], to)) {
//                    take.removeFromEnvelope(i, aCause);
//                }
//            }
//        }
    }
    private final IntProcedure pruneRefAndPickToOnTakeKer = new IntProcedure() {
        @Override
        public void execute(int takeKer) throws ContradictionException {
            PropUtil.intSubsetEnv(refs[takeKer], to, aCause);
            if (refs[takeKer].instantiated()) {
                to.addToKernel(refs[takeKer].getValue(), aCause);
            }
        }
    };
    private final IntProcedure pruneRefAndPickToOnToEnv = new IntProcedure() {
        @Override
        public void execute(int toEnv) throws ContradictionException {
            for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
                refs[i].removeValue(toEnv, aCause);
                if (refs[i].instantiated() && take.kernelContains(i)) {
                    to.addToKernel(refs[i].getValue(), aCause);
                }
            }
        }
    };
    private final IntProcedure pruneToOnRefRem = new IntProcedure() {
        @Override
        public void execute(int refRem) throws ContradictionException {
            for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
                if (refs[i].contains(refRem)) {
                    return;
                }
            }
            to.removeFromEnvelope(refRem, aCause);
        }
    };

    @Override
    public ESat isEntailed() {
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            if (!PropUtil.domainIntersectEnv(refs[i], to)) {
                return ESat.FALSE;
            }
        }
        if (!take.instantiated() || !to.instantiated()) {
            return ESat.UNDEFINED;
        }
        FixedCapacityIntSet values = new FixedCapacityIntSet(refs.length);
        for (IntVar ref : refs) {
            if (!ref.instantiated()) {
                return ESat.UNDEFINED;
            }
            values.add(ref.getValue());
        }
        return values.containsAll(to.getValue()) ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "joinFunction(" + take + ", " + Arrays.toString(refs) + ", " + to + ")";
    }
}
