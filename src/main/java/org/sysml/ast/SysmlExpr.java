package org.sysml.ast;

import java.io.IOException;

public interface SysmlExpr {
    /**
     * Dynamic dispatch on the visitor.
     *
     * @param <A> the parameter type
     * @param <B> the return type
     * @param visitor the visitor
     * @param a the parameter
     * @return the return value
     */
    <A, B> B accept(SysmlExprVisitor<A, B> visitor, A a) throws IOException;
}
