package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.IIntDeltaMonitor;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;
import org.chocosolver.util.procedure.IntProcedure;
import org.clafer.collection.MutableBoolean;

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
    private final ISetDeltaMonitor takeD;
    private final IntVar[] refs;
    private final IIntDeltaMonitor[] refsD;
    private final SetVar to;
    private final ISetDeltaMonitor toD;

    public PropJoinFunction(SetVar take, IntVar[] refs, SetVar to) {
        super(buildArray(take, to, refs), PropagatorPriority.QUADRATIC, true);
        this.take = take;
        this.takeD = take.monitorDelta(this);
        this.refs = refs;
        this.refsD = PropUtil.monitorDeltas(refs, this);
        this.to = to;
        this.toD = to.monitorDelta(this);
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

//    @Override
//    public boolean advise(int idxVarInProp, int mask) {
//        if (isRefVar(idxVarInProp)) {
//            return dontCare.contains(getRefVarIndex(idxVarInProp));
//        }
//        return super.advise(idxVarInProp, mask);
//    }
    @Override
    public int getPropagationConditions(int vIdx) {
        if (isTakeVar(vIdx) || isToVar(vIdx)) {
            return SetEventType.all();
        }
        assert isRefVar(vIdx);
        return IntEventType.all();
    }

    private boolean findMate(int toEnv) throws ContradictionException {
        boolean inKer = to.getLB().contains(toEnv);
        int mate = -1;
        ISetIterator iter = take.getUB().iterator();
        while (iter.hasNext()) {
            int j = iter.nextInt();
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
            to.remove(toEnv, this);
        } else if (mate != -2 && inKer) {
            // One mate.
            take.force(mate, this);
            return refs[mate].instantiateTo(toEnv, this);
        }
        return false;
    }

    private void findMates() throws ContradictionException {
        ISetIterator iter = to.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (findMate(i)) {
                findMates();
                return;
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune take
        ISetIterator takeEnv = take.getUB().iterator();
        while (takeEnv.hasNext()) {
            int i = takeEnv.nextInt();
            if (i < 0 || i >= refs.length || !PropUtil.isDomIntersectEnv(refs[i], to)) {
                take.remove(i, this);
            }
        }

        // Pick to and prune refs
        ISetIterator takeKer = take.getLB().iterator();
        while (takeKer.hasNext()) {
            int i = takeKer.nextInt();
            PropUtil.domSubsetEnv(refs[i], to, this);
            if (refs[i].isInstantiated()) {
                int value = refs[i].getValue();
                to.force(value, this);
            }
        }

        // Prune to
        findMates();
        takeD.unfreeze();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isTakeVar(idxVarInProp)) {
            takeD.freeze();
            takeD.forEach(this::pruneToOnTakeEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            takeD.forEach(this::pickToAndPruneChildOnTakeKer, SetEventType.ADD_TO_KER);
            takeD.unfreeze();
        } else if (isToVar(idxVarInProp)) {
            toD.freeze();
            toD.forEach(this::pruneRefOnToEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            toD.forEach(this::pickTakeOnToKer, SetEventType.ADD_TO_KER);
            toD.unfreeze();
            if (SetEventType.isEnvRemoval(mask)) {
                TIntArrayList removed = null;
                ISetIterator iter = take.getUB().iterator();
                while (iter.hasNext()) {
                    int i = iter.nextInt();
                    if (!PropUtil.isDomIntersectEnv(refs[i], to)) {
                        take.remove(i, this);
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
                            if (to.getUB().contains(i)) {
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
            IntProcedure pruneToOnRefRem = refRem -> {
                if (bool.isClear()) {
                    if (to.getUB().contains(refRem)) {
                        if (findMate(refRem)) {
                            bool.set();
                            findMates();
                        }
                    }
                }
            };
            refD.forEachRemVal(pruneToOnRefRem);
            refD.unfreeze();
            IntVar ref = refs[id];
            if (IntEventType.isRemove(mask)) {
                if (!PropUtil.isDomIntersectEnv(ref, to)) {
                    take.remove(id, this);
                }
            }
            if (ref.isInstantiated()) {
                if (take.getLB().contains(id)) {
                    to.force(ref.getValue(), this);
                }
            }
        }
    }

    private void pruneToOnTakeEnv(int takeEnv) throws ContradictionException {
        assert !take.getUB().contains(takeEnv);

        IntVar ref = refs[takeEnv];
        int ub = ref.getUB();
        for (int i = ref.getLB(); i <= ub; i = ref.nextValue(i)) {
            if (to.getUB().contains(i)) {
                if (findMate(i)) {
                    findMates();
                    return;
                }
            }
        }
    }

    private void pickToAndPruneChildOnTakeKer(int takeKer) throws ContradictionException {
        assert take.getLB().contains(takeKer);

        IntVar ref = refs[takeKer];
        PropUtil.domSubsetEnv(ref, to, this);
        if (ref.isInstantiated()) {
            to.force(ref.getValue(), this);
        }
    }

    private void pruneRefOnToEnv(int toEnv) throws ContradictionException {
        assert !to.getUB().contains(toEnv);

        ISetIterator iter = take.getLB().iterator();
        while (iter.hasNext()) {
            int takeKer = iter.nextInt();
            IntVar ref = refs[takeKer];
            if (ref.removeValue(toEnv, this) && ref.isInstantiated()) {
                to.force(ref.getValue(), this);
            }
        }
    }

    private void pickTakeOnToKer(int toVal) throws ContradictionException {
        assert to.getLB().contains(toVal);
        if (findMate(toVal)) {
            findMates();
        }
    }

    @Override
    public ESat isEntailed() {
        ISetIterator takeKer = take.getLB().iterator();
        while (takeKer.hasNext()) {
            int i = takeKer.nextInt();
            if (i < 0 || i >= refs.length || !PropUtil.isDomIntersectEnv(refs[i], to)) {
                return ESat.FALSE;
            }
            if (refs[i].isInstantiated()) {
                int value = refs[i].getValue();
                if (!to.getUB().contains(value)) {
                    return ESat.FALSE;
                }
            }
        }
        boolean completelyInstantiated = take.isInstantiated() && to.isInstantiated();
        int count = 0;
        IntVar[] taken = new IntVar[take.getUB().size()];
        ISetIterator takeEnv = take.getUB().iterator();
        while (takeEnv.hasNext()) {
            int i = takeEnv.nextInt();
            if (i >= 0 && i < refs.length) {
                IntVar ref = refs[i];
                completelyInstantiated = completelyInstantiated && ref.isInstantiated();
                taken[count++] = ref;
            }
        }
        if (count < taken.length) {
            taken = Arrays.copyOf(taken, count);
        }
        ISetIterator toKer = to.getLB().iterator();
        while (toKer.hasNext()) {
            int i = toKer.nextInt();
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
