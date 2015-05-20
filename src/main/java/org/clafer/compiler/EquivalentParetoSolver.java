package org.clafer.compiler;

import java.util.Arrays;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.search.solution.Solution;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;

/**
 * Takes an improvement-style optimizer that only returns one solution per
 * Pareto point and extends it to find all solutions per Pareto point.
 *
 * @author jimmy
 */
public class EquivalentParetoSolver implements ClaferOptimizer {

    private final AbstractImprovementOptimizer optimizer;
    private int count = 0;
    private boolean more = true;
    private Solution paretoSolution = null;
    private int paretoCount = 0;
    private Constraint[] equivalentConstraint;

    public EquivalentParetoSolver(AbstractImprovementOptimizer optimizer) {
        this.optimizer = Check.notNull(optimizer);
    }

    @Override
    public boolean find() {
        if (!more) {
            return false;
        }
        Solver solver = getInternalSolver();
        if (paretoSolution == null) {
            more &= optimizer.find();
            if (!more) {
                return false;
            }
            paretoCount = 1;
            paretoSolution = optimizer.solution();
        } else {
            if (paretoCount == 1) {
                solver.getEngine().flush();
                solver.getSearchLoop().reset();
                IntVar[] scores = optimizer.getScores();
                int[] paretoPoint = optimizer.optimalValues();
                equivalentConstraint = new Constraint[paretoPoint.length];
                for (int i = 0; i < scores.length; i++) {
                    equivalentConstraint[i] = ICF.arithm(scores[i], "=", paretoPoint[i]);
                    solver.post(equivalentConstraint[i]);
                }
            }
            boolean next = paretoCount == 1 ? solver.findSolution() : solver.nextSolution();
            if (solver.hasReachedLimit()) {
                more = false;
                throw new ReachedLimitException();
            }
            if (next) {
                paretoCount++;
            } else {
                for (Constraint c : equivalentConstraint) {
                    if (c != null) {
                        solver.unpost(c);
                    }
                }
                paretoSolution = null;
                return find();
            }
            if (duplicateSolution()) {
                return find();
            }
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
        return paretoCount == 1 ? optimizer.instance() : optimizer.getSolutionMap().getInstance();
    }

    @Override
    public int[] optimalValues() {
        return optimizer.optimalValues();
    }

    @Override
    public int instanceCount() {
        return count;
    }

    @Override
    public Solver getInternalSolver() {
        return optimizer.getInternalSolver();
    }

    private boolean duplicateSolution() {
        if (paretoSolution == null) {
            return false;
        }
        for (IntVar var : optimizer.getSolutionMap().getIrSolution().getIntVars()) {
            if ((var.getTypeAndKind() & Variable.CSTE) == 0) {
                if (var.getValue() != paretoSolution.getIntVal(var)) {
                    return false;
                }
            }
        }
        for (SetVar var : optimizer.getSolutionMap().getIrSolution().getSetVars()) {
            if ((var.getTypeAndKind() & Variable.CSTE) == 0) {
                if (!Arrays.equals(var.getValues(), paretoSolution.getSetVal(var))) {
                    return false;
                }
            }
        }
        return true;
    }
}
