package org.clafer.choco.constraint;

import solver.Solver;
import solver.exception.ContradictionException;
import solver.search.loop.monitors.IMonitorContradiction;
import solver.search.loop.monitors.IMonitorDownBranch;
import solver.variables.Variable;

/**
 *
 * @author jimmy
 */
public class ArcConsistentCheck implements IMonitorDownBranch, IMonitorContradiction {

    private final Solver solver;
    private final StringBuilder lastDecision = new StringBuilder();

    public ArcConsistentCheck(Solver solver) {
        this.solver = solver;
    }

    @Override
    public void beforeDownLeftBranch() {
        Variable[] vars = solver.getVars();
        lastDecision.setLength(0);
        lastDecision.append("Decision: ").append(solver.getSearchLoop().getLastDecision()).append('\n');
        lastDecision.append("Variables: ");
        for (Variable var : vars) {
            lastDecision.append(var).append(' ');
        }
    }

    @Override
    public void afterDownLeftBranch() {
    }

    @Override
    public void beforeDownRightBranch() {
        lastDecision.setLength(0);
    }

    @Override
    public void afterDownRightBranch() {
    }

    @Override
    public void onContradiction(ContradictionException cex) {
        if (lastDecision.length() > 0) {
            throw new Error("Not arc consistent: " + "\n" + lastDecision + "\n"+ cex);
        }
    }
}
