package org.clafer.ast;

import org.clafer.common.Check;

/**
 * An immutable constraint. Can be either hard or soft.
 *
 * @author jimmy
 */
public class AstConstraint {

    private final int id;
    private final AstClafer context;
    private AstBoolExpr expr;
    private final boolean soft;

    AstConstraint(int id, AstClafer context, AstBoolExpr expr) {
        this(id, context, expr, false);
    }

    AstConstraint(int id, AstClafer context, AstBoolExpr expr, boolean soft) {
        this.id = Check.notNull(id);
        this.context = Check.notNull(context);
        this.expr = Check.notNull(expr);
        this.soft = soft;
    }

    /**
     * Returns a unique identifier.
     *
     * @return a unique identifier
     */
    public int getId() {
        return id;
    }

    public AstClafer getContext() {
        return context;
    }

    public boolean isHard() {
        return !isSoft();
    }

    /**
     * Creates a new orphan constraint that is the same as this constraint
     * except it is a hard constraint. It is orphan because the Clafer defining
     * this constraint does not recognize the new constraint.
     *
     * @return an orphan hard version of this constraint
     */
    public AstConstraint asHard() {
        return new AstConstraint(id, context, expr, false);
    }

    public boolean isSoft() {
        return soft;
    }

    /**
     * Creates a new orphan constraint that is the same as this constraint
     * except it is a soft constraint. It is orphan because the Clafer defining
     * this constraint does not recognize the new constraint.
     *
     * @return an orphan soft version of this constraint
     */
    public AstConstraint asSoft() {
        return new AstConstraint(id, context, expr, true);
    }

    public AstBoolExpr getExpr() {
        return expr;
    }

    /**
     * Creates a new orphan constraint that is the same as this constraint
     * except it is a different expression. It is orphan because the Clafer
     * defining this constraint does not recognize the new constraint.
     *
     * @param expr the new expression
     * @return an orphan version of this constraint with a new expression
     */
    public AstConstraint withExpr(AstBoolExpr expr) {
        return new AstConstraint(id, context, expr, soft);
    }

    @Override
    public String toString() {
        return isHard() ? "[" + expr + "]" : "(" + expr + ")";
    }
}
