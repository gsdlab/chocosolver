package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import memory.IStateInt;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import util.ESat;

/**
 * Enforce no cycles.
 *
 * @author jimmy
 */
public class PropAcyclic extends Propagator<IntVar> {

    private final IStateInt[] leaders;

    /**
     * Enforce no cycles. {@code edges[i] = j} implies that there is a directed
     * edge from node i to node j. {@code edges[i] = -1} implies that there are
     * no direct edges from node i.
     *
     * @param edges
     */
    public PropAcyclic(IntVar[] edges) {
        super(edges, PropagatorPriority.TERNARY, true);
        this.leaders = new IStateInt[edges.length];
        for (int i = 0; i < this.leaders.length; i++) {
            this.leaders[i] = environment.makeInt(i);
        }
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return EventType.INSTANTIATE.mask;
    }

    private int getLeader(int node) {
        int leader = leaders[node].get();
        if (leader == node) {
            return node;
        }
        // Find the real leader.
        int realLeader = getLeader(leader);
        // Remember the real leader.
        if (realLeader != leader) {
            leaders[node].set(realLeader);
        }
        return leader;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = 0; i < vars.length; i++) {
            vars[i].removeValue(i, aCause);
            vars[i].updateLowerBound(-1, aCause);
            vars[i].updateUpperBound(vars.length - 1, aCause);
        }
        for (int i = 0; i < vars.length; i++) {
            if (vars[i].instantiated()) {
                follow(i, vars[i].getValue());
            }
        }
    }

    private void follow(int follower, int leader) throws ContradictionException {
        assert vars[follower].instantiated();
        if (leader == -1) {
            return;
        }
        int realLeader = getLeader(leader);
        if (realLeader == follower) {
            contradiction(vars[follower], "Cycle");
        }
        boolean changed = false;
        for (int i = 0; i < vars.length; i++) {
            if (getLeader(i) == follower) {
                assert vars[i].instantiated();
                leaders[i].set(realLeader);
                changed |= vars[realLeader].removeValue(i, aCause);
            }
        }
        leaders[follower].set(realLeader);
        if (changed && vars[realLeader].instantiated()) {
            follow(realLeader, vars[realLeader].getValue());
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        follow(idxVarInProp, vars[idxVarInProp].getValue());
    }

    @Override
    public ESat isEntailed() {
        // Hopefully escape analysis will make these boolean arrays cheap.
        boolean[] visited = new boolean[vars.length];
        boolean[] localVisited = new boolean[vars.length];
        boolean allInstantiated = true;
        for (int i = 0; i < vars.length; i++) {
            if (visited[i]) {
                continue;
            }
            if (vars[i].instantiated()) {
                Arrays.fill(localVisited, false);
                int cur = i;
                do {
                    if (localVisited[cur]) {
                        // Cycle
                        return ESat.FALSE;
                    }
                    visited[cur] = localVisited[cur] = true;
                    cur = vars[cur].getValue();
                    if (cur < -1 || cur >= vars.length) {
                        return ESat.FALSE;
                    }
                } while (cur != -1 && vars[cur].instantiated());
            } else {
                allInstantiated = false;
            }
        }
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "acyclic(" + Arrays.toString(vars) + ")";
    }
}
