package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrIntVar extends IrAbstractInt implements IrIntExpr, IrVar {

    private final String name;
    private final boolean decision;

    IrIntVar(String name, IrDomain domain) {
        this(name, domain, true);
    }

    IrIntVar(String name, IrDomain domain, boolean decision) {
        super(domain);
        this.name = Check.notNull(name);
        this.decision = decision;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public IrIntVar asDecision() {
        return new IrIntVar(name, getDomain(), true);
    }

    @Override
    public IrIntVar asNoDecision() {
        return new IrIntVar(name, getDomain(), false);
    }

    @Override
    public boolean isDecision() {
        return decision;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public String toString() {
        // TODO: return only name
        return name + "{domain=" + getDomain() + "}";
    }
}
