package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import java.util.Arrays;
import org.clafer.collection.FixedCapacityIntSet;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

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
    private final IntVar[] refs;
    private final SetVar to;

    public PropJoinFunction(SetVar take, IntVar[] refs, SetVar to) {
        super(buildArray(take, to, refs), PropagatorPriority.LINEAR, true);
        this.take = take;
        this.refs = refs;
        this.to = to;
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

    private void findMates() throws ContradictionException {
        for (int i = to.getEnvelopeFirst(); i != SetVar.END; i = to.getEnvelopeNext()) {
            if (findMate(i)) {
                findMates();
                return;
            }
        }
    }

    private int pruneRefsPickTo(boolean forceNoMoreDuplicates) throws ContradictionException {
        int sameRefs = 0;
        FixedCapacityIntSet set = new FixedCapacityIntSet(take.getKernelSize());
        // Prune refs, Pick to
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            PropUtil.intSubsetEnv(refs[i], to, aCause);
            if (refs[i].instantiated()) {
                int value = refs[i].getValue();
                sameRefs += set.add(value) ? 0 : 1;
                to.addToKernel(value, aCause);
            }
        }
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            if (!refs[i].instantiated()) {
                if (PropUtil.isDomainSubsetOf(refs[i], set)) {
                    sameRefs++;
                } else if (forceNoMoreDuplicates) {
                    TIntIterator iter = set.iterator();
                    while (iter.hasNext()) {
                        refs[i].removeValue(iter.next(), aCause);
                    }
                    if (refs[i].instantiated()) {
                        contradiction(refs[i], "Take too small");
                    }
                }
            }
        }
        return sameRefs;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune to
        findMates();

        // Prune refs, Pick to
        int sameRefs = pruneRefsPickTo(false);

        // Prune take
        for (int i = take.getEnvelopeFirst(); i != SetVar.END; i = take.getEnvelopeNext()) {
            if (!PropUtil.domainIntersectEnv(refs[i], to)) {
                take.removeFromEnvelope(i, aCause);
            }
        }

        int minTakeSize = sameRefs + to.getKernelSize();
        if (minTakeSize == take.getEnvelopeSize()) {
            take.instantiateTo(PropUtil.iterateEnv(take), aCause);
            to.instantiateTo(PropUtil.iterateKer(to), aCause);
            sameRefs = pruneRefsPickTo(true);
        }
        minTakeSize = sameRefs + to.getKernelSize();
        if (minTakeSize > take.getEnvelopeSize()) {
            contradiction(take, "Take too large");
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

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
