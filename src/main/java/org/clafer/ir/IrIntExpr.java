package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public interface IrIntExpr extends IrExpr {

    /**
     * Domain cannot be empty.
     * 
     * @return 
     */
    public IrDomain getDomain();

    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a);
}
