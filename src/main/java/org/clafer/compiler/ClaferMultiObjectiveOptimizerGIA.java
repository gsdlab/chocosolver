package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.search.strategy.Search;
import org.chocosolver.solver.variables.IntVar;

/**
 * Implementation of the guided improvement algorithm.
 *
 * @author jimmy
 */
public class ClaferMultiObjectiveOptimizerGIA extends AbstractImprovementOptimizer {

    private int count = 0;
    private boolean more = true;
    private final int[] optimalValues;
    // GIA
    private Solution solution = null;
    private final List<Constraint> stack = new ArrayList<>();
    private final IntVar[] bounds;
    private final Constraint dominate;

    ClaferMultiObjectiveOptimizerGIA(Solver solver, ClaferSolutionMap solutionMap,
            boolean[] maximizes, IntVar[] scores) {
        super(solver, solutionMap, maximizes, scores);
        this.optimalValues = new int[scores.length];
        this.bounds = new IntVar[scores.length];

        List<IntVar> boundVars = new ArrayList<>(this.bounds.length);
        List<Constraint> strictlyBetter = new ArrayList<>(this.bounds.length);
        for (int i = 0; i < this.bounds.length; i++) {
            IntVar score = scores[i];
            // TODO: copy the domain from score.
            IntVar bound = solver.getModel().intVar("bound" + i, score.getLB(), score.getUB());
            this.bounds[i] = bound;
            boundVars.add(bound);
            strictlyBetter.add(solver.getModel().arithm(bound, maximizes[i] ? "<" : ">", score));
        }
        this.dominate = or(strictlyBetter, solver.getModel());
        for (int i = 0; i < this.bounds.length; i++) {
            solver.getModel().arithm(this.bounds[i], maximizes[i] ? "<=" : ">=", scores[i]).post();
        }
        if (!boundVars.isEmpty()) {
            solver.setSearch(Search.sequencer(solver.getSearch(), Search.minDomLBSearch(
                    boundVars.toArray(new IntVar[boundVars.size()])
            )));
        }
    }

    private static Constraint or(List<Constraint> constraints, Model model) {
        return constraints.isEmpty()
                ? model.falseConstraint()
                : model.or(constraints.toArray(new Constraint[constraints.size()]));
    }

    private void push(Constraint constraint) {
        assert !stack.contains(constraint);
        constraint.post();
        stack.add(constraint);
    }

    private void popAll() {
        stack.forEach(solver.getModel()::unpost);
        stack.clear();
    }

    @Override
    public boolean find() throws ReachedLimitException {
        if (!more) {
            return false;
        }
        more &= count == 0 ? solveFirst() : solveNext();
        if (solver.isStopCriterionMet()) {
            more = false;
            if (solution != null) {
                InstanceModel bestInstance = solutionMap.getInstance(solution);
                int[] bestObjectiveValue = new int[scores.length];
                for (int i = 0; i < bestObjectiveValue.length; i++) {
                    bestObjectiveValue[i] = solution.getIntVal(scores[i]);
                }
                throw new ReachedLimitBestKnownException(bestInstance, bestObjectiveValue);
            }
            throw new ReachedLimitException();
        }
        if (more) {
            for (int i = 0; i < optimalValues.length; i++) {
                optimalValues[i] = solution.getIntVal(scores[i]);
            }
            count++;
        }
        return more;
    }

    /*
     * Implementation of multiple objective optimization based on discussion here:
     * https://github.com/chocoteam/choco3/issues/188.
     */
    private boolean solveFirst() {
        assert stack.isEmpty();

        solution = solver.findSolution();
        if (solution == null) {
            return false;
        }

        push(dominate);

        int[] best = new int[scores.length];
        Solution nextSolution = solution;
        do {
            solution = nextSolution;
            for (int i = 0; i < best.length; i++) {
                best[i] = scores[i].getValue();
            }

            for (int i = 0; i < bounds.length; i++) {
                push(solver.getModel().arithm(bounds[i], maximizes[i] ? ">=" : "<=", best[i]));
            }
        } while ((nextSolution = solver.findSolution()) != null);
        if (solver.isStopCriterionMet()) {
            return false;
        }
        popAll();
        for (int i = 0; i < bounds.length; i++) {
            push(solver.getModel().arithm(bounds[i], "=", scores[i]));
        }

        return true;
    }

    private boolean solveNext() {
        popAll();
        solver.reset();

        List<Constraint> strictlyBetter = new ArrayList<>(bounds.length);
        for (int i = 0; i < bounds.length; i++) {
            strictlyBetter.add(solver.getModel().arithm(bounds[i], maximizes[i] ? ">" : "<", optimalValues[i]));
        }
        or(strictlyBetter, solver.getModel()).post();
        return solveFirst();
    }

    @Override
    public InstanceModel instance() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutionMap.getInstance(solution);
    }

    @Override
    public Solution solution() {
        return solution;
    }

    @Override
    public int[] optimalValues() {
        if (count == 0) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return optimalValues.clone();
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
