package org.clafer.compiler;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstConstraint;
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
    private final Pair<AstConstraint, BoolVar>[] softVars;
    private final IntVar score;

    ClaferUnsat(Solver solver, ClaferSolutionMap solutionMap, Pair<AstConstraint, BoolVar>[] softVars, IntVar score) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        this.softVars = Check.noNulls(softVars);
        this.score = Check.notNull(score);
    }

    public Solver getInternalSolver() {
        return solver;
    }

    public Pair<Set<AstConstraint>, InstanceModel> minUnsat() {
        solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, score);
        if (ESat.TRUE.equals(solver.isFeasible())) {
            Set<AstConstraint> unsat = new HashSet<AstConstraint>();
            for (Pair<AstConstraint, BoolVar> softVar : softVars) {
                if (softVar.getSnd().instantiatedTo(0)) {
                    unsat.add(softVar.getFst());
                }
            }
            return new Pair<Set<AstConstraint>, InstanceModel>(unsat, solutionMap.getInstance());
        }
        return null;
    }
}
