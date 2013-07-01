package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrBoolVar extends IrIntVar implements IrBoolExpr, IrVar {

    private final boolean decision;

    IrBoolVar(String name, IrBoolDomain domain) {
        this(name, domain, true);
    }

    IrBoolVar(String name, IrBoolDomain domain, boolean decision) {
        super(name, domain);
        this.decision = decision;
    }

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
    public IrBoolVar asDecision() {
        return new IrBoolVar(getName(), getDomain(), true);
    }

    @Override
    public IrBoolVar asNoDecision() {
        return new IrBoolVar(getName(), getDomain(), false);
    }

    @Override
    public boolean isDecision() {
        return decision;
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
