package org.clafer.choco;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Settings;
import org.chocosolver.solver.Solver;
import org.chocosolver.util.criteria.Criterion;

/**
 * A solver whose stop criteria, for example a time limit, survive
 * {@link #reset()}. Choco's reset removes them, but the solvers in Clafer
 * reset the search to find further solutions under the same limits.
 *
 * @author Michał Antkiewicz
 */
public class ResetKeepsLimitsSolver extends Solver {

    private ResetKeepsLimitsSolver(Model model) {
        super(model);
    }

    /**
     * @return a new model whose solver keeps its stop criteria on reset
     */
    public static Model newModel() {
        return new Model(Settings.init().setInitSolver(ResetKeepsLimitsSolver::new));
    }

    @Override
    public void reset() {
        Criterion[] limits = criteria.toArray(new Criterion[criteria.size()]);
        super.reset();
        addStopCriterion(limits);
    }
}
