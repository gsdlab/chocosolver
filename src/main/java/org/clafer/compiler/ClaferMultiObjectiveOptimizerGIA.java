package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.collection.Either;
import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.constraints.LCF;
import org.chocosolver.solver.search.solution.Solution;
import org.chocosolver.solver.search.strategy.ISF;
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
            boolean[] maximizes, Either<Integer, IntVar>[] scores) {
        super(solver, solutionMap, maximizes, scores);
        this.optimalValues = new int[scores.length];
        this.bounds = new IntVar[scores.length];

        List<IntVar> boundVars = new ArrayList<>(this.bounds.length);
        List<Constraint> strictlyBetter = new ArrayList<>(this.bounds.length);
        for (int i = 0; i < this.bounds.length; i++) {
            if (scores[i].isRight()) {
                IntVar score = scores[i].getRight();
                IntVar bound = score.duplicate();
                this.bounds[i] = bound;
                boundVars.add(bound);
                strictlyBetter.add(ICF.arithm(bound, maximizes[i] ? "<" : ">", score));
            }
        }
        this.dominate = or(strictlyBetter, solver);
        for (int i = 0; i < this.bounds.length; i++) {
            if (this.bounds[i] != null) {
                solver.post(ICF.arithm(this.bounds[i], maximizes[i] ? "<=" : ">=", scores[i].getRight()));
            }
        }
        if (!boundVars.isEmpty()) {
            solver.set(ISF.sequencer(solver.getStrategy(), ISF.minDom_LB(
                    boundVars.toArray(new IntVar[boundVars.size()])
            )));
        }
    }

    private static Constraint or(List<Constraint> constraints, Solver solver) {
        return constraints.isEmpty()
                ? solver.FALSE
                : LCF.or(constraints.toArray(new Constraint[constraints.size()]));
    }

    private void push(Constraint constraint) {
        assert !stack.contains(constraint);
        solver.post(constraint);
        stack.add(constraint);
    }

    private void popAll() {
        assert solver.getSearchLoop().getCurrentDepth() == 0;
        stack.forEach(solver::unpost);
        stack.clear();
    }

    @Override
    public boolean find() throws ReachedLimitException {
        if (!more) {
            return false;
        }
        solution = new Solution();
        more &= count == 0 ? solveFirst() : solveNext();
        if (solver.hasReachedLimit()) {
            more = false;
            if (solution.hasBeenFound()) {
                InstanceModel bestInstance = solutionMap.getInstance(solution);
                int[] bestObjectiveValue = new int[scores.length];
                for (int i = 0; i < bestObjectiveValue.length; i++) {
                    bestObjectiveValue[i] = scores[i].isLeft()
                            ? scores[i].getLeft()
                            : solution.getIntVal(scores[i].getRight());
                }
                throw new ReachedLimitBestKnownException(bestInstance, bestObjectiveValue);
            }
            throw new ReachedLimitException();
        }
        if (more) {
            for (int i = 0; i < optimalValues.length; i++) {
                optimalValues[i] = scores[i].isLeft()
                        ? scores[i].getLeft()
                        : solution.getIntVal(scores[i].getRight());
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

        if (!solver.findSolution()) {
            return false;
        }

        push(dominate);

        int[] best = new int[scores.length];
        do {
            solution.record(solver);
            for (int i = 0; i < best.length; i++) {
                best[i] = scores[i].isLeft() ? scores[i].getLeft() : scores[i].getRight().getValue();
            }

            for (int i = 0; i < bounds.length; i++) {
                if (bounds[i] != null) {
                    push(ICF.arithm(bounds[i], maximizes[i] ? ">=" : "<=", best[i]));
                }
            }
        } while (solver.nextSolution());
        if (solver.hasReachedLimit()) {
            return false;
        }
        popAll();
        for (int i = 0; i < bounds.length; i++) {
            if (bounds[i] != null) {
                push(ICF.arithm(bounds[i], "=", scores[i].getRight()));
            }
        }

        return true;
    }

    private boolean solveNext() {
        popAll();
        solver.getEngine().flush();
        solver.getSearchLoop().reset();

        List<Constraint> strictlyBetter = new ArrayList<>(bounds.length);
        for (int i = 0; i < bounds.length; i++) {
            if (bounds[i] != null) {
                strictlyBetter.add(ICF.arithm(bounds[i], maximizes[i] ? ">" : "<", optimalValues[i]));
            }
        }
        solver.post(or(strictlyBetter, solver));
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
