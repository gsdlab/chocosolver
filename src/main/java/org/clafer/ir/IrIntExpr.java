package org.clafer.ir;

/**
 * An immutable expression that evaluates to an integer.
 * 
 * @author jimmy
 */
public interface IrIntExpr extends IrExpr {

    /**
     * Domain cannot be empty.
     * 
     * @return the domain of values this expression can take
     */
    public IrDomain getDomain();

    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a);
}
