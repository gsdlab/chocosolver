package org.clafer.choco.constraint;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.search.loop.monitors.IMonitorContradiction;
import org.chocosolver.solver.search.loop.monitors.IMonitorDownBranch;
import org.chocosolver.solver.variables.Variable;

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
    public void beforeDownBranch(boolean left) {
        if (left) {
            Variable[] vars = solver.getVars();
            lastDecision.setLength(0);
            lastDecision.append("Decision: ").append(solver.getSearchLoop().getLastDecision()).append('\n');
            lastDecision.append("Variables: ");
            for (Variable var : vars) {
                lastDecision.append(var).append(' ');
            }
        } else {
            lastDecision.setLength(0);
        }
    }

    @Override
    public void onContradiction(ContradictionException cex) {
        if (lastDecision.length() > 0) {
            throw new Error("Not arc consistent: " + "\n" + lastDecision + "\n" + cex);
        }
    }
}
