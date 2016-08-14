package org.clafer.compiler;

import org.chocosolver.solver.Model;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.Solver;

/**
 *
 * @author jimmy
 */
public class ClaferSolver implements ClaferSearch {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private final boolean restartAfterEachSolution;
    private int count = 0;
    private boolean more = true;

    ClaferSolver() {
        Model model = new Model();
        model.falseConstraint().post();
        this.solver = model.getSolver();
        this.solutionMap = null;
        this.restartAfterEachSolution = false;
    }

    ClaferSolver(Solver solver, ClaferSolutionMap solutionMap) {
        this(solver, solutionMap, false);
    }

    ClaferSolver(Solver solver, ClaferSolutionMap solutionMap, boolean restartAfterEachSolution) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        this.restartAfterEachSolution = restartAfterEachSolution;
    }

    public ClaferSolutionMap getSolutionMap() {
        return solutionMap;
    }

    @Override
    public boolean find() throws ReachedLimitException {
        if (!more) {
            return false;
        }
        if (restartAfterEachSolution) {
            solver.reset();
            more &= solver.solve();
        } else {
            more &= solver.solve();
        }
        if (solver.isStopCriterionMet()) {
            more = false;
            throw new ReachedLimitException();
        }
        if (more) {
            count++;
        }
        return more;
    }

    @Override
    public InstanceModel instance() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutionMap.getInstance();
    }

    @Override
    public int instanceCount() {
        return count;
    }

    @Override
    public Solver getInternalSolver() {
        return solver;
    }

    @Override
    public String toString() {
        return solver.toString();
    }
}
