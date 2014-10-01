package org.clafer.ir;

import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public interface IrIntArrayExpr extends IrExpr {

    public int length();

    public Domain[] getDomains();

    public <A, B> B accept(IrIntArrayExprVisitor<A, B> visitor, A a);
}
