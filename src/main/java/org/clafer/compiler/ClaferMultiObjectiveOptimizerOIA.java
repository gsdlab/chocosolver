package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.constraints.LCF;
import org.chocosolver.solver.search.solution.Solution;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.collection.Either;
import org.clafer.instance.InstanceModel;

/**
 *
 * @author jimmy
 */
public class ClaferMultiObjectiveOptimizerOIA extends AbstractImprovementOptimizer {

    private List<Solution> solutions = null;
    private int count = 0;

    ClaferMultiObjectiveOptimizerOIA(Solver solver, ClaferSolutionMap solutionMap,
            boolean[] maximizes, Either<Integer, IntVar>[] scores) {
        super(solver, solutionMap, maximizes, scores);
    }

    private boolean dominates(Solution a, Solution b) {
        // Assumes a and b are not equivalent.
        for (int i = 0; i < scores.length; i++) {
            if (scores[i].isRight()) {
                if (maximizes[i] && a.getIntVal(scores[i].getRight()) < b.getIntVal(scores[i].getRight())) {
                    return false;
                } else if (!maximizes[i] && a.getIntVal(scores[i].getRight()) > b.getIntVal(scores[i].getRight())) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public boolean find() throws ReachedLimitException {
        if (solutions == null) {
            solutions = new ArrayList<>();
            List<Constraint> stack = new ArrayList<>();
            if (solver.findSolution()) {
                do {
                    Solution solution = new Solution();
                    solution.record(solver);
                    solutions.removeIf(s -> dominates(solution, s));
                    solutions.add(solution);
                    List<Constraint> better = new ArrayList<>(scores.length);
                    for (int i = 0; i < scores.length; i++) {
                        if (scores[i].isRight()) {
                            better.add(ICF.arithm(
                                    scores[i].getRight(),
                                    maximizes[i] ? ">" : "<",
                                    scores[i].getRight().getValue()));
                        }
                    }
                    if (!better.isEmpty()) {
                        Constraint betterConstraint = LCF.or(better.toArray(new Constraint[better.size()]));
                        stack.add(betterConstraint);
                        solver.post(betterConstraint);
                    }
                } while (solver.nextSolution());
            }
            stack.forEach(solver::unpost);
            if (solver.hasReachedLimit()) {
                if (solutions.isEmpty()) {
                    throw new ReachedLimitException();
                }
                InstanceModel[] instances = new InstanceModel[solutions.size()];
                int[][] optimalValues = new int[solutions.size()][];
                int i = 0;
                for (Solution solution : solutions) {
                    instances[i] = solutionMap.getInstance(solution);
                    optimalValues[i] = new int[scores.length];
                    for (int j = 0; j < scores.length; j++) {
                        optimalValues[i][j] = scores[j].isLeft()
                                ? scores[j].getLeft()
                                : solution.getIntVal(scores[j].getRight());
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
            optimalValues[i] = scores[i].isLeft() ? scores[i].getLeft() : solution.getIntVal(scores[i].getRight());
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
