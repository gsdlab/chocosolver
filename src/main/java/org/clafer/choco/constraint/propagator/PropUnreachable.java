package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import memory.IStateInt;
import memory.IStateIntVector;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import util.ESat;

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
        this.from = from;
        this.to = to;
        this.position = solver.getEnvironment().makeInt(from);
        this.toComponent = solver.getEnvironment().makeIntVector(1, to);
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return EventType.INSTANTIATE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (from == to) {
            contradiction(vars[from], "trivial path");
        }
        for (int i = 0; i < vars.length; i++) {
            vars[i].updateLowerBound(0, aCause);
        }
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
            int i = 0;
            for (i = 0; i < vars.length && cur < vars.length && vars[cur].isInstantiated(); i++) {
                if (toComponent.contains(cur)) {
                    contradiction(vars[follower], "Reachable");
                }
                cur = vars[cur].getValue();
            }
            if (toComponent.contains(cur)) {
                contradiction(vars[follower], "Reachable");
            }
            if (cur >= vars.length || i == vars.length) {
                setPassive();
            } else {
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
        for (IntVar var : vars) {
            if (var.getUB() < 0) {
                return ESat.FALSE;
            }
        }
        // Hopefully escape analysis will make these boolean arrays cheap.
        boolean[] visited = new boolean[vars.length];
        int cur = from;
        while (cur < vars.length && vars[cur].isInstantiated() && !visited[cur]) {
            visited[cur] = true;
            if (cur == to) {
                return ESat.FALSE;
            }
            cur = vars[cur].getValue();
        }
        return cur >= vars.length || vars[cur].isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "unreachable(from=" + from + ", to=" + to + ", " + Arrays.toString(vars) + ")";
    }
}
