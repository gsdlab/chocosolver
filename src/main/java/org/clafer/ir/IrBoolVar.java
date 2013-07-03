package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrBoolVar extends IrIntVar implements IrBoolExpr, IrVar {

    IrBoolVar(String name, IrBoolDomain domain) {
        super(name, domain);
    }

    @Override
    public IrBoolDomain getDomain() {
        return (IrBoolDomain) super.getDomain();
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
