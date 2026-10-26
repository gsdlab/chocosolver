package org.clafer.choco;

import java.util.ArrayList;
import java.util.List;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.util.criteria.Criterion;

/**
 * Stop criteria, for example a time limit, that survive {@link #reset(Solver)}.
 * Choco's reset removes them, but the solvers in Clafer reset the search to
 * find further solutions under the same limits. Choco's solver is final, so
 * the limits are kept in a hook of its model and the solvers in Clafer reset
 * through {@link #reset(Solver)} instead of {@link Solver#reset()}.
 *
 * A time limit is a deadline: it counts from the call to
 * {@link #limitTime(Solver, long)}, not from the last reset, because Choco
 * restarts its clock on reset.
 *
 * @author Michał Antkiewicz
 */
public final class ResetKeepsLimits {

    private static final String LIMITS_HOOK = ResetKeepsLimits.class.getName();

    private ResetKeepsLimits() {
    }

    /**
     * Stop the search once {@code ms} milliseconds have passed, even across
     * resets.
     *
     * @param solver the solver
     * @param ms the time limit in milliseconds
     */
    public static void limitTime(Solver solver, long ms) {
        long deadline = System.nanoTime() + Math.min(ms, Long.MAX_VALUE / 2_000_000) * 1_000_000;
        Criterion limit = () -> System.nanoTime() - deadline >= 0;
        limits(solver.getModel()).add(limit);
        solver.addStopCriterion(limit);
    }

    /**
     * Reset the search and restore the limits set with
     * {@link #limitTime(Solver, long)}.
     *
     * @param solver the solver
     */
    public static void reset(Solver solver) {
        solver.reset();
        List<Criterion> limits = limits(solver.getModel());
        solver.addStopCriterion(limits.toArray(new Criterion[limits.size()]));
    }

    @SuppressWarnings("unchecked")
    private static List<Criterion> limits(Model model) {
        List<Criterion> limits = (List<Criterion>) model.getHook(LIMITS_HOOK);
        if (limits == null) {
            limits = new ArrayList<>();
            model.addHook(LIMITS_HOOK, limits);
        }
        return limits;
    }
}
