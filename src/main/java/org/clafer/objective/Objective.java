package org.clafer.objective;

import org.clafer.ast.AstSetExpr;
import org.clafer.common.Check;

/**
 * One objective.
 *
 * @author jimmy
 */
public class Objective {

    private static int idFactory = 0;
    private final int id = idFactory++;
    // true - maximize
    // false - minimize
    private final boolean maximize;
    private final AstSetExpr expr;

    Objective(boolean maximize, AstSetExpr expr) {
        this.maximize = maximize;
        this.expr = Check.notNull(expr);
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
        return new Objective(true, expression);
    }

    /**
     * Create a new minimization objective.
     *
     * @param expression the expression
     * @return minimize the expression
     */
    public static Objective minimize(AstSetExpr expression) {
        return new Objective(false, expression);
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
        return "<<" + (maximize ? "max " : "min ") + expr + ">>";
    }
}
