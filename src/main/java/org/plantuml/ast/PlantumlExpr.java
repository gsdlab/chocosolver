package org.plantuml.ast;

import java.io.IOException;

public interface PlantumlExpr {
    /**
     * Dynamic dispatch on the visitor.
     *
     * @param <A> the parameter type
     * @param <B> the return type
     * @param visitor the visitor
     * @param a the parameter
     * @return the return value
     */
    <A, B> B accept(PlantumlExprVisitor<A, B> visitor, A a) throws IOException;
}
