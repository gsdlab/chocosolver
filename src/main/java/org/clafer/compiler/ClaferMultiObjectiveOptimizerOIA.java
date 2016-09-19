package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.instance.InstanceModel;

/**
 *
 * @author jimmy
 */
public class ClaferMultiObjectiveOptimizerOIA extends AbstractImprovementOptimizer {

    private List<Solution> solutions = null;
    private int count = 0;

    ClaferMultiObjectiveOptimizerOIA(Solver solver, ClaferSolutionMap solutionMap,
            boolean[] maximizes, IntVar[] scores) {
        super(solver, solutionMap, maximizes, scores);
    }

    private boolean dominates(Solution a, Solution b) {
        // Assumes a and b are not equivalent.
        for (int i = 0; i < scores.length; i++) {
            if (maximizes[i] && a.getIntVal(scores[i]) < b.getIntVal(scores[i])) {
                return false;
            } else if (!maximizes[i] && a.getIntVal(scores[i]) > b.getIntVal(scores[i])) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean find() throws ReachedLimitException {
        if (solutions == null) {
            solutions = new ArrayList<>();
            List<Constraint> stack = new ArrayList<>();
            Solution solution;
            while ((solution = solver.findSolution()) != null) {
                final Solution currentSolution = solution;
                solutions.removeIf(s -> dominates(currentSolution, s));
                solutions.add(solution);
                List<Constraint> better = new ArrayList<>(scores.length);
                for (int i = 0; i < scores.length; i++) {
                    better.add(solver.getModel().arithm(
                            scores[i],
                            maximizes[i] ? ">" : "<",
                            scores[i].getValue()));
                }
                if (!better.isEmpty()) {
                    Constraint betterConstraint = solver.getModel().or(better.toArray(new Constraint[better.size()]));
                    stack.add(betterConstraint);
                    betterConstraint.post();
                }
            }
            stack.forEach(solver.getModel()::unpost);
            if (solver.isStopCriterionMet()) {
                if (solutions.isEmpty()) {
                    throw new ReachedLimitException();
                }
                InstanceModel[] instances = new InstanceModel[solutions.size()];
                int[][] optimalValues = new int[solutions.size()][];
                int i = 0;
                for (Solution bestKnown : solutions) {
                    instances[i] = solutionMap.getInstance(bestKnown);
                    optimalValues[i] = new int[scores.length];
                    for (int j = 0; j < scores.length; j++) {
                        optimalValues[i][j] = bestKnown.getIntVal(scores[j]);
                    }
                    i++;
                }
                throw new ReachedLimitBestKnownException(instances, optimalValues);
            }
        }
        return count++ < solutions.size();
    }

    @Override
    public InstanceModel instance() {
        if (solutions == null) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutionMap.getInstance(solutions.get(count - 1));
    }

    @Override
    public Solution solution() {
        if (solutions == null) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutions.get(count - 1);
    }

    @Override
    public int[] optimalValues() {
        Solution solution = solution();
        int[] optimalValues = new int[scores.length];
        for (int i = 0; i < optimalValues.length; i++) {
            optimalValues[i] = solution.getIntVal(scores[i]);
        }
        return optimalValues;
    }

    @Override
    public int instanceCount() {
        return count;
    }

    @Override
    public Solver getInternalSolver() {
        return solver;
    }
}
