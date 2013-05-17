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

    /**
     * Is the expression in negative form. Expressions in negated form have their
     * class names prefixed with "IrNot...", otherwise the expression is not negative.
     * 
     * @return true if the expression is in negative form, false otherwise
     */
    public boolean isNegative();

    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a);
}
