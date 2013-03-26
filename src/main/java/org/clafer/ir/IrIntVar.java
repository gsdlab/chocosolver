package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrIntVar implements IrIntExpr {

    private final String name;
    private final IrDomain domain;

    IrIntVar(String name, IrDomain domain) {
        this.name = name;
        this.domain = domain;
    }

    public String getName() {
        return name;
    }

    public IrDomain getDomain() {
        return domain;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return name;
    }
}
