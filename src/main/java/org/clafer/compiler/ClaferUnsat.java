package org.clafer.compiler;

import java.util.HashSet;
import java.util.Set;
import org.clafer.common.Check;
import org.clafer.collection.Pair;
import org.clafer.instance.InstanceModel;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class ClaferUnsat {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private final BoolVar[] softVars;
    private final IntVar score;

    ClaferUnsat(Solver solver, ClaferSolutionMap solutionMap, BoolVar[] softVars, IntVar score) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        this.softVars = Check.noNulls(softVars);
        this.score = Check.notNull(score);
    }

    public Solver getInternalSolver() {
        return solver;
    }

    public Pair<Set<String>, InstanceModel> minUnsat() {
        solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, score);
        if (ESat.TRUE.equals(solver.isFeasible())) {
            Set<String> unsat = new HashSet<String>();
            for (BoolVar softVar : softVars) {
                if (softVar.instantiatedTo(0)) {
                    unsat.add(softVar.getName());
                }
            }
            return new Pair<Set<String>, InstanceModel>(unsat, solutionMap.getInstance());
        }
        return null;
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
