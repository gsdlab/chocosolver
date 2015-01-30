package org.clafer.compiler;

import java.util.ArrayList;
import java.util.List;
import org.clafer.collection.Either;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.constraints.LCF;
import org.chocosolver.solver.search.strategy.ISF;
import org.chocosolver.solver.variables.IntVar;

/**
 * Implementation of the guided improvement algorithm.
 *
 * @author jimmy
 */
public class ClaferMultiObjectiveOptimizerGIA implements ClaferOptimizer {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private final boolean[] maximizes;
    private final Either<Integer, IntVar>[] scores;
    private int count = 0;
    private boolean more = true;
    private final int[] optimalValues;
    // GIA
    private final List<Constraint> stack = new ArrayList<>();
    private final IntVar[] bounds;
    private final Constraint dominate;

    ClaferMultiObjectiveOptimizerGIA(Solver solver, ClaferSolutionMap solutionMap,
            boolean[] maximize, Either<Integer, IntVar>[] scores) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        this.maximizes = maximize;
        this.scores = Check.noNullsNotEmpty(scores);
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
        for (Constraint constraint : stack) {
            solver.unpost(constraint);
        }
        stack.clear();
    }

    public ClaferSolutionMap getSolutionMap() {
        return solutionMap;
    }

    @Override
    public boolean find() {
        if (!more) {
            return false;
        }
        more &= count == 0 ? solveFirst() : solveNext();
        if (more) {
            if (count == 0) {
                for (int i = 0; i < optimalValues.length; i++) {
                    optimalValues[i] = scores[i].isLeft()
                            ? scores[i].getLeft()
                            : scores[i].getRight().getValue();
                }
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
            for (int i = 0; i < best.length; i++) {
                best[i] = scores[i].isLeft() ? scores[i].getLeft() : scores[i].getRight().getValue();
            }

            for (int i = 0; i < bounds.length; i++) {
                if (bounds[i] != null) {
                    push(ICF.arithm(bounds[i], maximizes[i] ? ">=" : "<=", best[i]));
                }
            }
        } while (solver.nextSolution());

        popAll();

        for (int i = 0; i < bounds.length; i++) {
            optimalValues[i] = best[i];
            if (bounds[i] != null) {
                push(ICF.arithm(bounds[i], "=", best[i]));
                push(ICF.arithm(scores[i].getRight(), "=", best[i]));
            }
        }

        solver.getEngine().flush();
        solver.getSearchLoop().reset();

        if (!solver.findSolution()) {
            throw new IllegalStateException("A solution is known to exist");
        }

        return true;
    }

    private boolean solveNext() {
        if (solver.nextSolution()) {
            return true;
        }

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
        return solutionMap.getInstance();
    }

    @Override
    public int[] optimalValues() {
        if (count == 0) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return optimalValues.clone();
    }

    @Override
    public InstanceModel[] allInstances() {
        List<InstanceModel> instances = new ArrayList<>();
        while (find()) {
            instances.add(instance());
        }
        return instances.toArray(new InstanceModel[instances.size()]);
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
