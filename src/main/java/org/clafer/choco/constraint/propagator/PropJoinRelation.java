package org.clafer.choco.constraint.propagator;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

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
    private final SetVar[] children;
    private final ISetDeltaMonitor[] childrenD;
    private final SetVar to;
    private final ISetDeltaMonitor toD;

    public PropJoinRelation(SetVar take, SetVar[] children, SetVar to) {
        super(buildArray(take, to, children), PropagatorPriority.QUADRATIC, true);
        this.take = take;
        this.takeD = take.monitorDelta(this);
        this.children = children;
        this.childrenD = PropUtil.monitorDeltas(children, this);
        this.to = to;
        this.toD = to.monitorDelta(this);
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
        return SetEventType.all();
    }

    private void findMate(int toEnv) throws ContradictionException {
        boolean inKer = to.getLB().contains(toEnv);
        int mate = -1;
        ISetIterator iter = take.getUB().iterator();
        while (iter.hasNext()) {
            int j = iter.nextInt();
            if (children[j].getUB().contains(toEnv)) {
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
            PropUtil.kerSubsetKer(children[mate], to, this);
            PropUtil.envSubsetEnv(children[mate], to, this);
            children[mate].force(toEnv, this);
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune take
        ISetIterator takeEnv = take.getUB().iterator();
        while (takeEnv.hasNext()) {
            int i = takeEnv.nextInt();
            if (i < 0 || i >= children.length || !PropUtil.isKerSubsetEnv(children[i], to)) {
                take.remove(i, this);
            }
        }

        // Pick to and prune child
        ISetIterator takeKer = take.getLB().iterator();
        while (takeKer.hasNext()) {
            int i = takeKer.nextInt();
            PropUtil.kerSubsetKer(children[i], to, this);
            PropUtil.envSubsetEnv(children[i], to, this);
        }

        // Pick take, pick child, pick take, prune to
        ISetIterator toEnv = to.getUB().iterator();
        while (toEnv.hasNext()) {
            int i = toEnv.nextInt();
            findMate(i);
        }
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
            toD.forEach(this::pruneChildOnToEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            toD.forEach(this::pickTakeOnToKer, SetEventType.ADD_TO_KER);
            toD.unfreeze();
            if (SetEventType.isEnvRemoval(mask)) {
                TIntArrayList removed = null;
                ISetIterator takeEnv = take.getUB().iterator();
                while (takeEnv.hasNext()) {
                    int i = takeEnv.nextInt();
                    if (!PropUtil.isKerSubsetEnv(children[i], to)) {
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
                        SetVar child = children[it.next()];
                        ISetIterator childEnv = child.getUB().iterator();
                        while (childEnv.hasNext()) {
                            int i = childEnv.nextInt();
                            if (to.getUB().contains(i)) {
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
            childD.forEach(this::pruneToOnChildEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            if (take.getLB().contains(id)) {
                childD.forEach(this::pickToOnChildKer, SetEventType.ADD_TO_KER);
            } else if (take.getUB().contains(id)) {
                childD.forEach(childKer -> {
                    if (!to.getUB().contains(childKer)) {
                        take.remove(id, this);
                        ISetIterator childEnv = children[id].getUB().iterator();
                        while (childEnv.hasNext()) {
                            int i = childEnv.nextInt();
                            if (to.getUB().contains(i)) {
                                findMate(i);
                            }
                        }
                    }
                }, SetEventType.ADD_TO_KER);
            }
            childD.unfreeze();
        }
    }

    private void pruneToOnTakeEnv(int takeEnv) throws ContradictionException {
        assert !take.getUB().contains(takeEnv);
        ISetIterator iter = children[takeEnv].getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (to.getUB().contains(i)) {
                findMate(i);
            }
        }
    }

    private void pickToAndPruneChildOnTakeKer(int takeKer) throws ContradictionException {
        assert take.getLB().contains(takeKer);

        SetVar child = children[takeKer];
        PropUtil.kerSubsetKer(child, to, this);
        PropUtil.envSubsetEnv(child, to, this);
    }

    private void pruneToOnChildEnv(int childEnv) throws ContradictionException {
        // Note the the child may no longer be in take, but still need to find mate.
        // For example:
        //     take = {0,1,2}, child0 = {0}, child1 = {1}, child2 = {2}, to = {0,1,2}
        //   remove 2 from take and 2 from child2
        //     take = {0,1}, child0 = {0}, child1 = {1}, child2 = {}, to = {0,1,2}
        // Need to find mate for 2 on child2 or else to will keep 2, and break
        // idempotency.
        if (to.getUB().contains(childEnv)) {
            findMate(childEnv);
        }
    }

    private void pickToOnChildKer(int childKer) throws ContradictionException {
        // assert id in ker(take)
        to.force(childKer, this);
    }

    private void pruneChildOnToEnv(int toEnv) throws ContradictionException {
        assert !to.getUB().contains(toEnv);

        ISetIterator iter = take.getLB().iterator();
        while (iter.hasNext()) {
            children[iter.nextInt()].remove(toEnv, this);
        }
    }

    private void pickTakeOnToKer(int toVal) throws ContradictionException {
        assert to.getLB().contains(toVal);
        findMate(toVal);
    }

    @Override
    public ESat isEntailed() {
        ISetIterator takeKer = take.getLB().iterator();
        while (takeKer.hasNext()) {
            int i = takeKer.nextInt();
            if (i < 0 || i >= children.length) {
                return ESat.FALSE;
            }
            ISetIterator childKer = children[i].getLB().iterator();
            while (childKer.hasNext()) {
                int j = childKer.nextInt();
                if (!to.getUB().contains(j)) {
                    return ESat.FALSE;
                }
            }
        }
        boolean completelyInstantiated = take.isInstantiated() && to.isInstantiated();
        int count = 0;
        SetVar[] taken = new SetVar[take.getUB().size()];
        ISetIterator takeEnv = take.getUB().iterator();
        while (takeEnv.hasNext()) {
            int i = takeEnv.nextInt();
            if (i >= 0 && i < children.length) {
                SetVar child = children[i];
                completelyInstantiated = completelyInstantiated && child.isInstantiated();
                taken[count++] = child;
            }
        }
        if (count < taken.length) {
            taken = Arrays.copyOf(taken, count);
        }
        ISetIterator toKer = to.getLB().iterator();
        while (toKer.hasNext()) {
            int i = toKer.nextInt();
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
