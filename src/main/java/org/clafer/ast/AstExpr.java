package org.clafer.ast;

import java.io.Serializable;

/**
 *
 * @author jimmy
 */
public interface AstExpr extends Serializable {

    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a);
}
