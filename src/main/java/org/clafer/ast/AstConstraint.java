package org.clafer.ast;

import org.clafer.common.Check;

/**
 * An immutable constraint. Can be either hard or soft.
 *
 * @author jimmy
 */
public class AstConstraint {

    private static int idFactory = 0;
    private final int id = idFactory++;
    private final AstClafer context;
    private AstBoolExpr expr;
    private final boolean soft;

    public AstConstraint(AstClafer context, AstBoolExpr expr) {
        this(context, expr, false);
    }

    public AstConstraint(AstClafer context, AstBoolExpr expr, boolean soft) {
        this.context = Check.notNull(context);
        this.expr = Check.notNull(expr);
        this.soft = soft;
    }

    public AstClafer getContext() {
        return context;
    }

    public boolean isHard() {
        return !isSoft();
    }

    public boolean isSoft() {
        return soft;
    }

    public AstBoolExpr getExpr() {
        return expr;
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
        return isHard() ? "[" + expr + "]" : "(" + expr + ")";
    }
}
