package org.clafer.ir;

/**
 * An immutable expression that evaluates to a boolean.
 * 
 * @author jimmy
 */
public interface IrBoolExpr extends IrExpr {

    /**
     * @return the domain of values this expression can take
     */
    public IrBoolDomain getDomain();

    /**
     * The negated expression is true if and only if this expression if false.
     * 
     * @return negated expression
     */
    public IrBoolExpr negate();

    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a);
}
