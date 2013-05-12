package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrSetExpr extends IrExpr {

    public IrDomain getEnv();

    public IrDomain getKer();

    public IrDomain getCard();

    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a);
}
