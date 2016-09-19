package org.clafer.assertion;

import java.util.concurrent.atomic.AtomicInteger;
import org.clafer.ast.AstBoolExpr;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class Assertion {

    private static final AtomicInteger idFactory = new AtomicInteger();
    private final int id = idFactory.getAndIncrement();
    private final AstBoolExpr expr;

    public Assertion(AstBoolExpr expr) {
        this.expr = Check.notNull(expr);
    }

    /**
     * Returns the expression to assert.
     *
     * @return the expression to assert
     */
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
        return "assert " + expr;
    }
}
