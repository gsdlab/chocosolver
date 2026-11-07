package org.clafer.assertion;

import java.util.concurrent.atomic.AtomicInteger;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.SourceSpan;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class Assertion {

    private static final AtomicInteger idFactory = new AtomicInteger();
    private final int id = idFactory.getAndIncrement();
    private final AstBoolExpr expr;
    private final SourceSpan span;

    public Assertion(AstBoolExpr expr) {
        this(expr, null);
    }

    /**
     * @param expr the expression to assert
     * @param span where the assertion is in the source model, or {@code null}
     * if unknown
     */
    public Assertion(AstBoolExpr expr, SourceSpan span) {
        this.expr = Check.notNull(expr);
        this.span = span;
    }

    /**
     * Returns the expression to assert.
     *
     * @return the expression to assert
     */
    public AstBoolExpr getExpr() {
        return expr;
    }

    public boolean hasSpan() {
        return span != null;
    }

    /**
     * Returns where the assertion is in the source model.
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
        return "assert " + expr + (span == null ? "" : " at " + span);
    }
}
