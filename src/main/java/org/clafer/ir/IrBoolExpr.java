package org.clafer.ir;

/**
 * An immutable expression that evaluates to a boolean.
 * 
 * @author jimmy
 */
public interface IrBoolExpr extends IrBool, IrExpr {

    /**
     * The negated expression is true if and only if this expression if false.
     * 
     * @return negated expression
     */
    public IrBoolExpr negate();

    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a);
}
