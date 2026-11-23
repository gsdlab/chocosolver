package org.clafer.ir;

import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public interface IrSetArrayExpr extends IrArrayExpr {

    public Domain[] getEnvs();

    public Domain[] getKers();

    public Domain[] getCards();

    public <A, B> B accept(IrSetArrayExprVisitor<A, B> visitor, A a);
}
