package org.clafer.ast;

import java.util.concurrent.atomic.AtomicInteger;
import org.clafer.common.Check;

/**
 * An immutable constraint. Can be either hard or soft.
 *
 * @author jimmy
 */
public class AstConstraint {

    private static final AtomicInteger idFactory = new AtomicInteger();
    private final int id = idFactory.getAndIncrement();
    private final AstClafer context;
    private final AstBoolExpr expr;
    private final boolean soft;
    private final SourceSpan span;

    public AstConstraint(AstClafer context, AstBoolExpr expr) {
        this(context, expr, false);
    }

    public AstConstraint(AstClafer context, AstBoolExpr expr, boolean soft) {
        this(context, expr, soft, null);
    }

    /**
     * @param context the Clafer the constraint is under
     * @param expr the constraint
     * @param soft whether the constraint is soft
     * @param span where the constraint is in the source model, or {@code null}
     * if unknown
     */
    public AstConstraint(AstClafer context, AstBoolExpr expr, boolean soft, SourceSpan span) {
        this.context = Check.notNull(context);
        this.expr = Check.notNull(expr);
        this.soft = soft;
        this.span = span;
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

    public boolean hasSpan() {
        return span != null;
    }

    /**
     * @return where the constraint is in the source model, or {@code null} if
     * unknown
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
        String constraint = isHard() ? "[" + expr + "]" : "(" + expr + ")";
        return span == null ? constraint : constraint + " at " + span;
    }
}
