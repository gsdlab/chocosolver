package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.memory.IStateInt;
import org.chocosolver.memory.IStateIntVector;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 * Enforces no path from one node to another.
 *
 * @author jimmy
 */
public class PropUnreachable extends Propagator<IntVar> {

    private final int from;
    private final int to;
    // The end of the path.
    private final IStateInt position;
    // All the nodes that reach the end node.
    private final IStateIntVector toComponent;

    /**
     * Enforce no path from one node to another. {@code edges[i] = j} implies
     * that there is a directed edge from node i to node j.
     * {@code edges[i] ≥ edges.length} implies that there are no direct edges
     * from node i.
     *
     * @param edges the edges
     * @param from the start node
     * @param to the end node
     */
    public PropUnreachable(IntVar[] edges, int from, int to) {
        super(edges, PropagatorPriority.TERNARY, true);
        if (from < 0 || from >= vars.length) {
            throw new IllegalArgumentException();
        }
        if (to < 0 || to >= vars.length) {
            throw new IllegalArgumentException();
        }
        this.from = from;
        this.to = to;
        this.position = solver.getEnvironment().makeInt(from);
        this.toComponent = solver.getEnvironment().makeIntVector(1, to);
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return IntEventType.instantiation();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (from == to) {
            contradiction(vars[from], "trivial path");
        }
        vars[from].updateLowerBound(0, aCause);
        vars[from].removeValue(to, aCause);
        for (int i = 0; i < vars.length; i++) {
            if (vars[i].isInstantiated() && !isPassive()) {
                follow(i);
            }
        }
    }

    private void remove(IntVar var, IStateIntVector remove) throws ContradictionException {
        int size = remove.size();
        for (int i = 0; i < size; i++) {
            var.removeValue(remove.get(i), aCause);
        }
    }

    private void follow(int follower) throws ContradictionException {
        assert vars[follower].isInstantiated();
        int leader = vars[follower].getValue();
        if (position.get() == follower) {
            int cur = leader;
            if (cur < 0) {
                contradiction(vars[follower], "Reachable");
            }
            int i;
            for (i = 0; i < vars.length && cur < vars.length && vars[cur].isInstantiated(); i++) {
                if (toComponent.contains(cur)) {
                    contradiction(vars[follower], "Reachable");
                }
                cur = vars[cur].getValue();
                if (cur < 0) {
                    contradiction(vars[follower], "Reachable");
                }
            }
            if (toComponent.contains(cur)) {
                contradiction(vars[follower], "Reachable");
            }
            if (cur >= vars.length || i == vars.length) {
                setPassive();
            } else {
                vars[cur].updateLowerBound(0, aCause);
                remove(vars[cur], toComponent);
                position.set(cur);
                if (vars[cur].isInstantiated()) {
                    follow(cur);
                }
            }
        } else if (leader < vars.length && !toComponent.contains(follower) && toComponent.contains(leader)) {
            toComponent.add(follower);
            vars[position.get()].removeValue(follower, aCause);
            for (int i = 0; i < vars.length; i++) {
                if (vars[i].isInstantiatedTo(follower)) {
                    follow(i);
                }
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        follow(idxVarInProp);
    }

    @Override
    public ESat isEntailed() {
        // Hopefully escape analysis will make these boolean arrays cheap.
        boolean[] visited = new boolean[vars.length];
        int cur = from;
        while (cur < vars.length && vars[cur].isInstantiated() && !visited[cur]) {
            visited[cur] = true;
            if (cur == to) {
                return ESat.FALSE;
            }
            cur = vars[cur].getValue();
            if (cur < 0) {
                return ESat.FALSE;
            }
        }
        return cur >= vars.length || vars[cur].isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "unreachable(from=" + from + ", to=" + to + ", " + Arrays.toString(vars) + ")";
    }
}
