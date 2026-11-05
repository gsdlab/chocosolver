package org.clafer.objective;

import java.util.concurrent.atomic.AtomicInteger;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.SourceSpan;
import org.clafer.common.Check;

/**
 * One objective. Use either
 * {@link Objective#maximize(org.clafer.ast.AstSetExpr)} or
 * {@link Objective#minimize(org.clafer.ast.AstSetExpr)} to construct a new
 * objective.
 *
 * @author jimmy
 */
public class Objective {

    private static final AtomicInteger idFactory = new AtomicInteger();
    private final int id = idFactory.getAndIncrement();
    // true - maximize
    // false - minimize
    private final boolean maximize;
    private final AstSetExpr expr;
    private final SourceSpan span;

    private Objective(boolean maximize, AstSetExpr expr, SourceSpan span) {
        this.maximize = maximize;
        this.expr = Check.notNull(expr);
        this.span = span;
    }

    /**
     * Check if this objective is a maximization objective.
     *
     * @return {@code true} if the objective is to maximize the expression,
     * {@code false} otherwise
     */
    public boolean isMaximize() {
        return maximize;
    }

    /**
     * Check if this objective is a minimization objective.
     *
     * @return {@code true} if the objective is to minimize the expression,
     * {@code false} otherwise
     */
    public boolean isMinimize() {
        return !maximize;
    }

    /**
     * Returns the expression to optimize.
     *
     * @return the expression to optimize
     */
    public AstSetExpr getExpr() {
        return expr;
    }

    /**
     * Create a new maximization objective.
     *
     * @param expression the expression
     * @return maximize the expression
     */
    public static Objective maximize(AstSetExpr expression) {
        return maximize(expression, null);
    }

    /**
     * Create a new maximization objective.
     *
     * @param expression the expression
     * @param span where the objective is in the source model, or {@code null}
     * if unknown
     * @return maximize the expression
     */
    public static Objective maximize(AstSetExpr expression, SourceSpan span) {
        return new Objective(true, expression, span);
    }

    /**
     * Create a new minimization objective.
     *
     * @param expression the expression
     * @return minimize the expression
     */
    public static Objective minimize(AstSetExpr expression) {
        return minimize(expression, null);
    }

    /**
     * Create a new minimization objective.
     *
     * @param expression the expression
     * @param span where the objective is in the source model, or {@code null}
     * if unknown
     * @return minimize the expression
     */
    public static Objective minimize(AstSetExpr expression, SourceSpan span) {
        return new Objective(false, expression, span);
    }

    public boolean hasSpan() {
        return span != null;
    }

    /**
     * Returns where the objective is in the source model.
     *
     * @return the source span, or {@code null} if unknown
     */
    public SourceSpan getSpan() {
        return span;
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public String toString() {
        return "<<" + (maximize ? "max " : "min ") + expr + ">>" + (span == null ? "" : " at " + span);
    }
}
