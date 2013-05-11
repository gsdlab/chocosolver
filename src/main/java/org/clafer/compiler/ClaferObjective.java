package org.clafer.compiler;

import org.clafer.Check;
import org.clafer.instance.InstanceModel;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.search.measure.IMeasures;
import solver.variables.IntVar;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class ClaferObjective {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private final Objective objective;
    private final IntVar score;

    ClaferObjective(Solver solver, ClaferSolutionMap solutionMap, Objective objective, IntVar score) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        this.objective = Check.notNull(objective);
        this.score = Check.notNull(score);
    }

    public IMeasures getMeasures() {
        return solver.getMeasures();
    }

    public Objective getObjective() {
        return objective;
    }

    public InstanceModel optimal() {
        solver.findOptimalSolution(objective.getPolicy(), score);
        return ESat.TRUE.equals(solver.isFeasible()) ? solutionMap.getInstance() : null;
    }

    public static enum Objective {

        Maximize(ResolutionPolicy.MAXIMIZE),
        Minimize(ResolutionPolicy.MINIMIZE);
        private final ResolutionPolicy policy;

        Objective(ResolutionPolicy policy) {
            this.policy = policy;
        }

        ResolutionPolicy getPolicy() {
            return policy;
        }
    }
}
